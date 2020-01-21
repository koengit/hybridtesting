-- An evaluator. Can be used with Val.
{-# LANGUAGE DefaultSignatures, TupleSections, FlexibleInstances #-}
module Process.Eval where

import Data.Map(Map)
import qualified Data.Map.Strict as Map
import Control.Monad
import Data.Functor.Identity
import Process.Language
import Process.Pretty()
import Text.PrettyPrint.HughesPJClass
import qualified Val

--------------------------------------------------------------------------------

type Env f = Map Var (f Value)

data Value
  = DoubleValue{ doubleValue :: Double }
  | BoolValue{ boolValue :: Bool }
 deriving (Eq, Ord)

constant :: Value -> Expr
constant (DoubleValue x) = Double x
constant (BoolValue   x) = Bool x

instance Show Value where
  show (DoubleValue x) = show x
  show (BoolValue x) = show x

--------------------------------------------------------------------------------

class Valued f where
  val         :: a -> f a
  vmap        :: Ord b => (a -> b) -> f a -> f b
  vlift       :: Ord c => (a -> b -> c) -> f a -> f b -> f c
  vifThenElse :: Ord a => f Bool -> f a -> f a -> f a
  vprune      :: Ord a => f a -> f a
  vfail       :: String -> f a

instance Valued Identity where
  val               = return
  vmap              = fmap
  vlift             = liftM2
  vifThenElse c a b = if runIdentity c then a else b
  vprune            = id
  vfail s           = error s

instance Valued Maybe where
  val                      = return
  vmap                     = fmap
  vlift                    = liftM2
  vifThenElse (Just c) a b = if c then a else b
  vifThenElse Nothing  a b = Nothing
  vprune                   = id
  vfail _s                 = Nothing

instance Valued Val.Val where
  val         = Val.val
  vmap        = Val.mapVal
  vlift       = Val.liftVal
  vifThenElse = Val.ifThenElse
  vprune      = Val.forget
  vfail s     = error s

--------------------------------------------------------------------------------

eval :: Valued f => Env f -> Expr -> f Value
eval env (Var x) =
  case Map.lookup x env of
    Nothing -> vfail ("variable " ++ show x ++ " not bound")
    Just v  -> v

eval _ (Double x) =
  val (DoubleValue x)

eval env (Plus e1 e2) =
  DoubleValue `vmap` vlift (+)
    (doubleValue `vmap` eval env e1)
    (doubleValue `vmap` eval env e2)

eval env (Times e1 e2) =
  DoubleValue `vmap` vlift (*)
    (doubleValue `vmap` eval env e1)
    (doubleValue `vmap` eval env e2)

eval env (Power e1 e2) =
  DoubleValue `vmap` vlift (**)
    (doubleValue `vmap` eval env e1)
    (doubleValue `vmap` eval env e2)

eval env (Negate e) =
  (DoubleValue . negate . doubleValue) `vmap` eval env e

eval env (Not e) =
  (BoolValue . not . boolValue) `vmap` eval env e

eval env (And e1 e2) =
  BoolValue `vmap` vlift (&&)
    (boolValue `vmap` eval env e1)
    (boolValue `vmap` eval env e2)

eval _ (Bool x) =
  val (BoolValue x)

eval env (Positive e) =
  (BoolValue . (>= 0) . doubleValue) `vmap` eval env e

eval env (Zero e) =
  (BoolValue . (== 0) . doubleValue) `vmap` eval env e

eval _ e =
  vfail $ show $
    sep [
      text "don't know how to evaluate",
      nest 2 (pPrint e),
      text "(try using 'lower stdPrims' before simulating)"]

--------------------------------------------------------------------------------

execStep :: Valued f => Env f -> Step -> Env f
execStep env p = Map.union (go p) env
 where
  go (If e s1 s2)     = Map.unionWith (vifThenElse (boolValue `vmap` eval env e))
                          (go s1)
                          (go s2)
  go (Update m)       = Map.map (eval env) m
  go (Assume str e s) = Map.alter (alt e) Pre  (go s)
  go (Assert str e s) = Map.alter (alt e) Post (go s)
    
  alt e Nothing  = Just (eval env e)
  alt e (Just w) = Just (BoolValue `vmap` vlift (&&) (boolValue `vmap` eval env e)
                                                     (boolValue `vmap` w))

--------------------------------------------------------------------------------

simulate :: Valued f => Double -> [Env f] -> Process -> [Env f]
simulate delta inputs process = go (execStep empty (start process)) inputs
 where
  empty = Map.fromList
          [ (Delta, val (DoubleValue delta))
          , (Pre,   val (BoolValue True))
          , (Post,  val (BoolValue True))
          ]

  go state []         = []
  go state (inp:inps) = state' : go (Map.map vprune state') inps
   where
    state' = execStep (Map.union inp state) (step process)

simulateReal :: Double -> [Env Identity] -> Process -> [Env Identity]
simulateReal = simulate

simulateVal :: Double -> [Env Identity] -> Process -> [Env Val.Val]
simulateVal delta inputs process = simulate delta inputs' process
 where
  inputs' = map (Map.map (val . runIdentity)) inputs

--------------------------------------------------------------------------------

