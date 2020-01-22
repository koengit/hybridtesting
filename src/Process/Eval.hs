-- An evaluator. Can be used with Val.
{-# LANGUAGE DefaultSignatures, TupleSections, FlexibleInstances #-}
module Process.Eval where

import Data.Map(Map)
import qualified Data.Map as Map
import qualified Data.Map.Merge.Lazy as Merge
import Control.Monad
import Data.Functor.Identity
import Process.Language
import Process.Pretty()
import Text.PrettyPrint.HughesPJClass
import qualified Val

--------------------------------------------------------------------------------

type Env f = Map Var (f Value)

data Value
  = DoubleValue{ doubleValue :: Double}
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
  vfail       :: Ord a => String -> f a

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
  vfail s     = Val.mkVal [] -- error s

vlift3 :: (Valued f, Ord a, Ord b, Ord d) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
vlift3 f x y z =
  vlift (uncurry f) (vlift (,) x y) z

--------------------------------------------------------------------------------

{-
data EVal a
  = Val (Val.Val a)
  | Err String
 deriving ( Eq, Ord )

instance Show a => Show (EVal a) where
  show (Val v) = show v
  show (Err s) = show s

instance Valued EVal where
  val            = Val . val
  vmap f (Val v) = Val (vmap f v)
  
  vlift f (Err s) _       = Err s
  vlift f _       (Err s) = Err s
  vlift f (Val x) (Val y) = Val (vlift f x y)
  
  vifThenElse (Err s) _ _ = Err s
  vifThenElse (Val b) x y = 

  vprune (Val v) = Val (vprune v)
  vprune (Err s) = Err s
  
  vfail s = Err s
-}

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

execStep :: (Show (f Bool), Valued f) => Env f -> Step -> Env f
execStep env p = Map.unionWithKey h (go p) env
 where
  go (If e s1 s2)     = iff (boolValue `vmap` eval env e) (go s1) (go s2)
  go (Update m)       = Map.map (eval env) m
  go (Assume str e s) = add Pre  (eval env e) (go s)
  go (Assert str e s) = add Post (eval env e) (go s)
    
  iff c =
    Merge.merge (Merge.mapMaybeMissing fxv)
                (Merge.mapMaybeMissing fxw)
                (Merge.zipWithMaybeMatched f)
   where
    fxv x v = Just (case Map.lookup x env of
                      Just w  -> vifThenElse c v w
                      Nothing -> v {- !!: x is undefined in this branch -})
    fxw x w = Just (case Map.lookup x env of
                      Just v  -> vifThenElse c v w
                      Nothing -> w {- !!: x is undefined in this branch -})
    f x v w = Just (vifThenElse c v w)

  add x v =
    Map.insertWith (&&&) x v

  h x new old
    | x == Pre || x == Post = new &&& old
    | otherwise             = new

  v &&& w =
    BoolValue `vmap` vlift (&&) (boolValue `vmap` v) (boolValue `vmap` w)

--------------------------------------------------------------------------------

emptyEnv :: Valued f => Double -> Env f
emptyEnv delta =
  Map.fromList
  [ (Delta, val (DoubleValue delta))
  , (Pre,   val (BoolValue True))
  , (Post,  val (BoolValue True))
  ]

--------------------------------------------------------------------------------

simulate :: (Show (f Bool), Valued f) => Double -> [Env f] -> Process -> [Env f]
simulate delta inputs process =
  go (execStep (emptyEnv delta) (start process)) inputs
 where
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

