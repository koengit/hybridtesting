-- An evaluator. Can be used with Val.
{-# LANGUAGE DefaultSignatures, TupleSections, FlexibleInstances, DeriveGeneric #-}
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
import GHC.Generics
import Data

--------------------------------------------------------------------------------

type Env f = Map Var (f Value)

data Value
  = DoubleValue{ doubleValue :: Double}
  | BoolValue{ boolValue :: Bool }
 deriving (Eq, Ord, Generic)

instance Data Value

constant :: Value -> Expr
constant (DoubleValue x) = Double x
constant (BoolValue   x) = Bool x

instance Show Value where
  show (DoubleValue x) = show x
  show (BoolValue x) = show x

--------------------------------------------------------------------------------

class Valued f where
  val         :: a -> f a
  vthe        :: f a -> Maybe a
  vpositive   :: f Double -> f Bool
  vzero       :: f Double -> f Bool
  vmap        :: Ord b => (a -> b) -> f a -> f b
  vlift       :: Ord c => (a -> b -> c) -> f a -> f b -> f c
  vbind       :: Ord b => f a -> (a -> f b) -> f b
  vifThenElse :: Ord a => f Bool -> f a -> f a -> f a
  vshare      :: f a -> f (f a)
  vfail       :: Ord a => String -> f a
  vforget     :: Int -> f a -> f a
  vplot       :: f Value -> Double

instance Valued Identity where
  val               = return
  vthe              = Just . runIdentity
  vpositive         = vmap (>= 0)
  vzero             = vmap (== 0)
  vmap              = fmap
  vlift             = liftM2
  vbind             = (>>=)
  vifThenElse c a b = if runIdentity c then a else b
  vshare            = return
  vfail s           = error s
  vforget _         = id
  vplot v           = case runIdentity v of
                        DoubleValue x -> x
                        BoolValue b   -> if b then 1 else 0

instance Valued Maybe where
  val                      = return
  vthe                     = id
  vpositive                = vmap (>= 0)
  vzero                    = vmap (== 0)
  vmap                     = fmap
  vlift                    = liftM2
  vbind                    = (>>=)
  vifThenElse (Just c) a b = if c then a else b
  vifThenElse Nothing  a b = Nothing
  vshare                   = return
  vfail _s                 = Nothing
  vforget _                = id
  vplot Nothing            = 0
  vplot (Just x)           = vplot (return x :: Identity Value)

instance Valued Val.Val where
  val         = Val.val
  vthe        = Just . Val.the
  vpositive   = (Val.>=? 0)
  vzero       = (Val.==? 0)
  vmap        = Val.mapVal
  vlift       = Val.liftVal
  vbind       = Val.bindVal
  vifThenElse = Val.ifThenElse
  vshare      = Val.share
  vfail s     = error s
  vforget     = Val.forget
  vplot v     = case Val.the v of
                  DoubleValue x -> x
                  BoolValue _   -> Val.howTrue (boolValue `vmap` v)

vlift3 :: (Valued f, Ord a, Ord b, Ord d) => (a -> b -> c -> d) -> f a -> f b -> f c -> f d
vlift3 f x y z =
  vlift (uncurry f) (vlift (,) x y) z

vmapM :: (Valued f, Ord b) => (a -> f b) -> [a] -> f [b]
vmapM _ [] = val []
vmapM f (x:xs) = vlift (:) (f x) (vmapM f xs)

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
  BoolValue `vmap` vpositive (doubleValue `vmap` eval env e)

eval env (Zero e) =
  BoolValue `vmap` vzero (doubleValue `vmap` eval env e)

eval env (Cond c e1 e2) =
  vifThenElse (boolValue `vmap` eval env c) (eval env e1) (eval env e2)
  
eval _ e =
  vfail $ show $
    sep [
      text "don't know how to evaluate",
      nest 2 (pPrint e),
      text "(try using 'lower stdPrims' before simulating)"]

--------------------------------------------------------------------------------

-- the semantics of this function has changed, Pre and Post are not accumulative
-- anymore!
execStep :: (Valued f, Ord (f Value)) => Env f -> Step -> Env f
execStep env0 (Step p) =
  Map.map forgetIt $ Map.union (Map.map (evalShared env) p) env
 where
  env   = Map.union reset env0
  reset = Map.fromList
          [ (Pre,  val (BoolValue True))
          , (Post, val (BoolValue True))
          ]
  evalShared env e =
    shareEnv env e `vbind` flip eval e
  shareEnv env e =
    vmap Map.fromList (vmapM sharePair (Map.toList (Map.filterWithKey p env)))
    where
      p x _ = x `elem` vars e
  sharePair (x, v) =
    vmap (x,) (vshare v)
  forgetIt val =
    case vthe val of
      Just (BoolValue _) -> vforget 2 val
      _ -> vforget 3 val
  
--------------------------------------------------------------------------------

emptyEnv :: Valued f => Double -> Env f
emptyEnv delta =
  Map.fromList
  [ (Delta, val (DoubleValue delta))
  ]

--------------------------------------------------------------------------------

simulate :: (Valued f, Ord (f Value)) => Double -> [Env f] -> Process -> [Env f]
simulate delta inputs process =
  go (execStep (emptyEnv delta `Map.union` head inputs) (start process)) inputs
 where
  go state []         = [state]
  go state (inp:inps) = state : go state' inps
   where
    state' = execStep (Map.union inp state) (step process)

simulateReal :: Double -> [Env Identity] -> Process -> [Env Identity]
simulateReal = simulate

simulateVal :: Double -> [Env Identity] -> Process -> [Env Val.Val]
simulateVal delta inputs process = simulate delta inputs' process
 where
  inputs' = map (Map.map (val . runIdentity)) inputs

--------------------------------------------------------------------------------

