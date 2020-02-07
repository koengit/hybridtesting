-- Combinators for building processes and expressions.
module Process.Combinators where

import Process.Language
import Process.Simplify
import qualified Data.Map.Strict as Map
import Data.Generics.Uniplate.Data
import qualified Data.Set as Set
import Data.List
import Data.Maybe
import Utils

----------------------------------------------------------------------
-- Combinators for building processes.
----------------------------------------------------------------------

-- Make a simple process
process :: Step -> Step -> Process
process start step =
  Process {
    locals = 0,
    start = start,
    step = step }

-- Do something only on the first step, or only after the first step
first, loop :: Step -> Process
first p = process p skip
loop p = process skip p

-- Parallel composition
class Par a where
  -- A process that does nothing
  skip :: a
  -- Execute two processes in parallel
  (&) :: a -> a -> a

infixr 5 &

-- Execute many processes in parallel
par :: Par a => [a] -> a
par = foldr (&) skip

instance Par Step where
  skip = Step Map.empty

  Step m1 & Step m2 =
    Step (Map.unionWithKey f m1 m2)
    where
      f Pre x y = x &&& y
      f Post x y = x &&& y
      f _ x y
        | x == y = x
        | otherwise = error "Step.&: incoherent state update"

instance Par Process where
  skip = process skip skip
  (&) = combine (&) (&)

-- Combining two alternatives with if-then-else
class Ite a where
  ite :: Expr -> a -> a -> a

instance Ite Expr where
  ite = Cond

instance Ite Step where
  ite c (Step m1) (Step m2) =
    Step (Map.mergeWithKey f g h m1 m2)
    where
      f _ e1 e2 = Just (Cond c e1 e2)
      g = Map.mapWithKey (\x e1 -> Cond c e1 (Var x))
      h = Map.mapWithKey (\x e2 -> Cond c (Var x) e2)

instance Ite Process where
  ite c = combine (ite c) (ite c)

-- Generate a local name
name :: String -> (Var -> Process) -> Process
name s f = p{locals = locals p + 1}
  where
    p = f n
    n = Local s (locals p)

-- Update a variable
set :: Var -> Expr -> Step
set x e = Step (Map.singleton x e)

-- Assumptions and assertions
assume, assert :: String -> Expr -> Step
assume str e = set Pre e
assert str e = set Post e

-- Define a variable whose value changes with every step
continuous :: Var -> Expr -> Expr -> Process
continuous x start step =
  process (set x start) (set x step)

-- Choose between two processes. Initial state executes both
switch :: Expr -> Process -> Process -> Process
switch cond p1 p2 =
  combine (&) (ite cond) p1 p2

-- Sequential composition
sequential :: Process -> Expr -> Process -> Process
sequential p e q =
  name "b" $ \x ->
    combine
      (\_ _ -> (start p & q1 & set x (bool False)))
      (\_ _ ->
        (ite (var x)
          (step q)
          (ite e (set x (bool True) & q2)
            (step p))))
      p q
  where
    -- Make sure that all variables get initialised in the first time-step
    q1 =
      Step $
      Map.filterWithKey
        (\x _ -> x `notElem` [Pre, Post] ++ Map.keys (updates (start p)))
        (updates (start q))
    
    -- Variables written by p must be initialised only once q starts for real
    -- (Note: no need to check step p because start p is required to
    -- initialised all variables in p)
    q2 =
      Step $
      Map.filterWithKey (\x _ -> x `elem` [Pre, Post] || x `Map.member` updates (start p))
        (updates (start q))

wait :: Expr -> Process -> Process
wait e p = sequential skip e p

----------------------------------------------------------------------
-- Combinators for building expressions
----------------------------------------------------------------------

var :: Var -> Expr
var = Var

delta :: Expr
delta = Var Delta

double :: Double -> Expr
double = Double

nott :: Expr -> Expr
nott = Not

infixr 3 &&&
(&&&) :: Expr -> Expr -> Expr
(&&&) = And

infixr 2 |||
(|||) :: Expr -> Expr -> Expr
(|||) x y = nott (nott x &&& nott y)

infixr 0 ==>
(==>) :: Expr -> Expr -> Expr
x ==> y = nott x ||| y

bool :: Bool -> Expr
bool = Bool

true, false :: Expr
true = bool True
false = bool False

infix 4 ==?, >=?, <=?, /=?
(==?), (>=?), (<=?) :: Expr -> Expr -> Expr
x ==? y = Zero (x-y)
x /=? y = Not (x ==? y)
x >=? y = Positive (x-y)
x <=? y = y >=? x

cond :: Expr -> Expr -> Expr -> Expr
cond = Cond

-- Primitive functions
primitive :: PrimitiveKind -> String -> [Param] -> [Expr] -> Expr
primitive = Primitive

minn, maxx :: Expr -> Expr -> Expr
minn x y = primitive Functional "min" [] [x, y]
maxx x y = primitive Functional "max" [] [x, y]

between :: Expr -> (Expr, Expr) -> Expr
e `between` (x, y) = x <=? e &&& e <=? y

old :: Expr -> Expr -> Expr
old initial x = primitive Temporal "old" [] [initial, x]

clampedIntegral :: Expr -> Expr -> Expr -> Expr
clampedIntegral e lo hi = primitive Temporal "clampedIntegral" [] [e, lo, hi]

integralReset :: Expr -> Expr -> Expr
integralReset e reset = primitive Temporal "integral" [] [e, reset]

integral :: Expr -> Expr
integral e = integralReset e false

smartIntegral :: Expr -> Expr
smartIntegral = linear f
  where
    f (Primitive Temporal "deriv" [] [e]) = e
    f e = integral e

deriv :: Expr -> Expr
deriv e = primitive Temporal "deriv" [] [e]

derivDegree :: Var -> Expr -> Int
derivDegree x e =
  maximum (mapMaybe deg (universeBi e))
  where
    deg (Var y) | x == y = Just 0
    deg (Primitive Temporal "deriv" [] [e]) = fmap succ (deg e)
    deg _ = Nothing

linear :: (Expr -> Expr) -> Expr -> Expr
linear f (Plus e1 e2) = Plus (linear f e1) (linear f e2)
linear f (Negate e) = Negate (linear f e)
linear f (Times (Double k) e) = Times (Double k) (linear f e)
linear f (Times e (Double k)) = Times (linear f e) (Double k)
linear f e = f e

-- Desugarings for the primitives
stdPrims :: [(String, Prim)]
stdPrims = temporalPrims ++ functionalPrims

temporalPrims :: [(String, Prim)]
temporalPrims =
  [("deriv",
    \_ [e] k ->
      -- old delta is the time difference from the previous state to this
      k ((e - old 0 e) * old 0 (1 / delta))),
   ("integral",
    \_ [e, reset] k ->
      name "x" $ \x ->
        let e' = cond reset 0 (var x + delta * e) in
        continuous x 0 e' &
        -- x is the *old* value of the integral, so e' is the current value
        k e'),
   ("clampedIntegral",
    \_ [e, lo, hi] k ->
      name "x" $ \x ->
        let e' = clamp lo hi (var x + delta * e) in
        continuous x 0 e' &
        k e'),
   ("old",
    \_ [initial, e] k ->
      name "w" $ \x ->
        -- Works because all updates are done simultaneously
        continuous x initial e & k (Var x))]

functionalPrims :: [(String, Prim)]
functionalPrims =
  [("min",
    \_ [x, y] k -> k (Cond (x <=? y) x y)),
   ("max",
    \_ [x, y] k -> k (Cond (x >=? y) x y)),
   ("interpolate2d",
    \[xs, ys, pss] [x, y] k ->
      let
        unArray (Array xs) = xs
        unScalar (Scalar x) = Double x
        xs'  = map unScalar (unArray xs)
        ys'  = map unScalar (unArray ys)
        pss' = map (map unScalar) (map unArray (unArray pss))
      in 
        k (evalInterpolate2d xs' ys' pss' (x, y)))]

-- Clamp a value to a range
clamp :: Expr -> Expr -> Expr -> Expr
clamp lo hi x = maxx lo (minn hi x)

-- Define a variable by means of a differential equation in t.
differentialEquation :: Var -> Expr -> Expr -> Process
differentialEquation x initial e =
  continuous x initial (solve (Var x) (foldn (derivDegree x e) smartIntegral e))

-- Given e = L(y)/L(x),
-- transferFunction y x s e finds the differential equation defining y.
transferFunction :: Var -> Var -> Var -> Expr -> Expr
transferFunction y x s e =
  -- L(y)/L(x) = k*num/denom
  -- L(y)*denom = k*L(x)*num
  inverseLaplace s (var y * product denom - Double k * var x * product num)
  where
    (k, num, denom) = factors' e

-- Given e = L(x),
-- laplaceFunction x s e finds the differential equation defining x.
laplaceFunction :: Var -> Var -> Expr -> Expr
laplaceFunction x s e =
  replaceGlobal (deriv (Double 0)) (Double 0) $
  replaceGlobal (deriv (Double 1)) (Double 0) $
  replaceGlobal (deriv (var t)) (Double 1) $
  transferFunction x t s e
  where
    t = Global "laplaceT"

-- Try to invert the Laplace transform. Only handles differential equations.
inverseLaplace :: Var -> Expr -> Expr
inverseLaplace s = simplifyExpr . linear eliminate . expand . simplifyExpr
  where
    eliminate e
      | s `Set.notMember` vars e = e
      | var s `elem` es =
        Double k * deriv (eliminate (product (es \\ [var s])))
      | otherwise = error "couldn't eliminate s from Laplace transform"
      where
        (k, es) = factors e

interpolate2d :: [Double] -> [Double] -> [[Double]] -> (Expr, Expr) -> Expr
interpolate2d xs ys pss (x, y) =
  primitive Functional "interpolate2d"
    [Array (map Scalar xs),
     Array (map Scalar ys),
     Array (map Array (map (map Scalar) pss))]
    [x, y]

evalInterpolate2d :: [Expr] -> [Expr] -> [[Expr]] -> (Expr, Expr) -> Expr
evalInterpolate2d xs ys pss (x, y) =
  findPair (zip xs pss) x $ \(x1, ps) (x2, qs) ->
  findPair (zip ys (zip ps qs)) y $ \(y1,  (z11, z21)) (y2, (z12, z22)) ->
    (z11*(x2-x)*(y2-y) + z21*(x-x1)*(y2-y) + z12*(x2-x)*(y-y1) + z22*(x-x1)*(y-y1))/((x2-x1)*(y2-y1))

  where
    findPair :: [(Expr, a)] -> Expr -> ((Expr, a) -> (Expr, a) -> Expr) -> Expr
    findPair [p1, p2] x k = k p1 p2
    findPair (p1:p2:ps) x k =
      cond (x <=? fst p2)
        (k p1 p2)
        (findPair (p2:ps) x k)
