{-# LANGUAGE RecordWildCards, PatternGuards #-}
module Flowstar.Translate where

import qualified Flowstar.Model as F
import qualified Process as P
import qualified Process.Language as P
import qualified Data.Map as Map
import qualified Data.Set as Set
import Data.Maybe
import Control.Monad

types :: F.Model -> P.Types
types F.Model{..} =
  Map.mapKeys (P.Global . ("init_" ++)) $
  Map.fromSet (\v -> (P.Parameter, P.Real (defaultBounds (bounds v initialVars)))) vars

defaultBounds :: (Maybe Double, Maybe Double) -> (Double, Double)
defaultBounds (Just min, Just max) = (min, max)
defaultBounds (Just min, Nothing)  = (min, min + 100)
defaultBounds (Nothing,  Just max) = (max - 100, max)
defaultBounds (Nothing,  Nothing)  = (-100, 100)

toPositive :: F.Constraint -> [P.Expr]
toPositive (e `F.In` (x, y)) =
  [expr e - P.double x,
   P.double y - expr e]
toPositive (F.Zero e) =
  [expr e, - expr e]
toPositive (F.Positive e) =
  [expr e]

bounds :: F.Var -> [F.Constraint] -> (Maybe Double, Maybe Double)
bounds v constraints =
  foldr combine (Nothing, Nothing) (map (bound v) constraints)
  where
    bound v (F.Positive e) =
      let (a, es1, es2) = P.terms' (expr e) in
      case P.factors (sum (es1 ++ map negate es2)) of
        (b, [P.Var (P.Global w)]) | v == w ->
          -- b*w + a >= 0 => b*w >= -a
          -- => w >= -a/b if b > 0,
          --    w <= -a/b if b < 0
          if b > 0
            then (Just (-a/b), Nothing)
            else (Nothing, Just (-a/b))
        _ -> (Nothing, Nothing)

    bound v (e `F.In` (x, y)) =
      -- e >= x, y >= e
      combine
        (bound v (F.Positive (F.Plus e (F.Negate (F.Const x)))))
        (bound v (F.Positive (F.Plus (F.Const y) (F.Negate e))))

    bound v (F.Zero e) =
      bound v (e `F.In` (0, 0))

    combine (x1, y1) (x2, y2) = (comb max x1 x2, comb min y1 y2)
      where
        comb _ Nothing x = x
        comb _ x Nothing = x
        comb f (Just x) (Just y) = Just (f x y)

expr :: F.Expr -> P.Expr
expr (F.Var x) = P.var (P.Global x)
expr (F.Const x) = P.double x
expr (F.Plus x y) = expr x + expr y
expr (F.Times x y) = expr x * expr y
expr (F.Power x y) = P.Power (expr x) (expr y)
expr (F.Negate x) = negate (expr x)
expr (F.Sin _) = error "sin not supported"
expr (F.Interval _ _) = error "interval not supported"

constraint :: F.Constraint -> P.Expr
constraint (F.Zero e) = expr e P.==? 0
constraint (F.Positive e) = expr e P.>=? 0
constraint (e `F.In` (x, y)) = expr e P.>=? P.double x P.&&& expr e P.<=? P.double y

-- Convert a constraint of the form e=0
-- into e <= 0, if we know e >= 0,
-- or   e >= 0, if we know e <= 0.
robustConstraint :: F.Constraint -> [F.Constraint] -> P.Expr
robustConstraint (F.Zero e) cs
  | Just e' <- msum (map weaken1 (concatMap toPositive cs)) = e'
  where
    weaken1 e'
      | P.terms' (e' - expr e) == (0, [], []) =
        -- e' =  e ==> e >= 0 so (e = 0 <=> e <= 0)
        Just (expr e P.<=? 0)
      | P.terms' (e' + expr e) == (0, [], []) =
        -- e' = -e ==> e <= 0 so (e = 0 <=> e >= 0)
        Just (expr e P.>=? 0)
      | otherwise = Nothing
robustConstraint (e `F.In` (x, y)) cs | x == y =
  robustConstraint (F.Zero (e `F.Plus` F.Const (negate x))) cs
robustConstraint e _ = constraint e

model :: F.Model -> P.Process
model F.Model{..} =
  P.name "state" $ \state ->
    let
      modeNums = Map.fromList (zip (Map.keys modes) (map P.double [1..]))
      modeNum x = Map.findWithDefault undefined x modeNums
      start =
        P.set state (modeNum initialMode) P.&
        P.par [P.set x e | (x, e) <- subst] P.&
        P.par [P.assume "initial constraint" (P.substitute subst (constraint c)) | c <- initialVars]
      step = foldr modeIte (P.assert "invalid mode" P.false) (Map.toList modes)
      modeIte (name, val) rest =
        P.ite (P.var state P.==? modeNum name)
          (mode (\name -> P.set state (modeNum name)) val)
          rest
      subst =
        [(P.Global x, P.var (P.Global ("init_" ++ x))) | x <- Set.toList vars]
    in P.process start step

mode :: (F.ModeName -> P.Step) -> F.Mode -> P.Step
mode jump F.Mode{..} =
  -- The plan: never let the system run into a bad state.
  -- To do so, we check if the update we are planning to do
  -- would move the system into a state where it should jump.
  -- If it would, then we do the jump.
  --
  -- When we jump, the variables are still updated, because the
  -- current values often will not satisfy the invariant for the
  -- post-jump state (e.g.: current state has invariant x <= 1, next
  -- state has invariant x >= 1).
  --
  -- As a last tweak, reset variables take their values from the
  -- *current* state (this makes the bouncing ball example work and
  -- doesn't hurt the other examples).
  P.par [assumption c | c <- invariant] P.&
  isSafe P.&
  foldr addJump (update next) jumps
    where
      assumption c = P.assume "invariant" (constraint c)
      isSafe =
        case unsafe of
          Nothing -> P.skip
          Just unsafe ->
            P.assert "unsafe" $ P.nott $
              foldr (P.&&&) P.true $
              [robustConstraint c invariant | c <- unsafe]

      addJump F.Jump{..} p =
        P.ite
          (P.substitute next
            (foldr (P.&&&) P.true [robustConstraint c invariant | c <- condition]))
          (jump target P.&
           (update $
             [(P.Global x, expr e) | (x, e) <- Map.toList reset] ++
             [(P.Global x, e) | (P.Global x, e) <- next, x `Map.notMember` reset]))
          p

      update xs = P.par [P.set x e | (x, e) <- xs]

      next =
        [ (P.Global x, P.var (P.Global x) + expr e * P.delta)
        | (x, e) <- Map.toList derivatives ]
