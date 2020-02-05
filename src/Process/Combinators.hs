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

instance Par Process where
  skip = Process 0 Map.empty
  (&) = combine (Map.unionWithKey f)
    where
      f Pre x y  = Stream (start x &&& start y) (step x &&& step y)
      f Post x y = Stream (start x &&& start y) (step x &&& step y)
      f _ x y
        | x == y = x
        | otherwise = error "Step.&: incoherent state update"

-- Generate a local name
name :: String -> (Var -> Process) -> Process
name s f = p{locals = locals p + 1}
  where
    p = f n
    n = Local s (locals p)

-- Update a variable
define :: Var -> Stream -> Process
define x s = Process 0 (Map.singleton x s)

-- Assumptions and assertions
assume, assert :: String -> Stream -> Process
assume _ = define Pre
assert _ = define Post

-- Define a variable whose value changes with every step
continuous :: Expr -> Expr -> Stream
continuous start step = Stream start step

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
between e (min, max) = e >=? min &&& e <=? max

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
        define x (continuous 0 e') &
        -- x is the *old* value of the integral, so e' is the current value
        k e'),
   ("clampedIntegral",
    \_ [e, lo, hi] k ->
      name "x" $ \x ->
        let e' = clamp lo hi (var x + delta * e) in
        define x (continuous 0 e') &
        k e'),
   ("old",
    \_ [initial, e] k ->
      name "w" $ \x ->
        -- Works because all updates are done simultaneously
        define x (continuous initial e) & k (Var x))]

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
  define x (continuous initial (solve (Var x) (foldn (derivDegree x e) smartIntegral e)))

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
