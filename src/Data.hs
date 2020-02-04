{-# LANGUAGE FlexibleContexts, DefaultSignatures, TypeOperators, FlexibleInstances, MultiParamTypeClasses #-}
module Data where

{-
Varying the data-part of test data by means of expressing them as numerical data.
-}

import Test.QuickCheck
import Data.List
import Optimize
import GHC.Generics
import qualified Data.Map as M
import Data.Proxy

--------------------------------------------------------------------------------

class RealFrac d => Data d a where
  vals :: a -> [d]
  fill :: a -> [d] -> a

  default vals :: (Generic a, GData d (Rep a)) => a -> [d]
  vals = genericVals

  default fill :: (Generic a, GData d (Rep a)) => a -> [d] -> a
  fill = genericFill

instance RealFrac d => Data d ()
instance (Data d a, Data d b) => Data d (a, b)
instance (Data d a, Data d b) => Data d (Either a b)
instance Data d a => Data d [a]

instance RealFrac d => Data d Bool

instance RealFrac d => Data d d where
  vals x       = [x]
  fill _ (v:_) = v

instance RealFrac d => Data d Int where
  vals n       = [fromIntegral n]
  fill _ (v:_) = round v

instance (Ord a, Data d b) => Data d (M.Map a b) where
  vals m    = [ x | (_,y) <- M.toList m, x <- vals y ]
  fill m vs = M.fromList (xs `zip` fill ys vs)
   where
    (xs,ys) = unzip (M.toList m)

--------------------------------------------------------------------------------

genericVals :: (Generic a, GData d (Rep a)) => a -> [d]
genericVals = gvals undefined . from

genericFill :: (Generic a, GData d (Rep a)) => a -> [d] -> a
genericFill x xs = to (gfill undefined (from x) xs)

class GData d f where
  gvals :: proxy d -> f a -> [d]
  gfill :: proxy d -> f a -> [d] -> f a

instance (GData d f, GData d g) => GData d (f :*: g) where
  gvals p (x :*: y)    = gvals p x ++ gvals p y
  gfill p (x :*: y) vs = gfill p x (take k vs) :*: gfill p y (drop k vs)
   where
    k = length (gvals p x)

instance (GData d f, GData d g) => GData d (f :+: g) where
   gvals p (L1 x) = gvals p x
   gvals p (R1 y) = gvals p y

   gfill p (L1 x) = L1 . gfill p x
   gfill p (R1 y) = R1 . gfill p y

instance GData d f => GData d (M1 i c f) where
  gvals p (M1 x) = gvals p x
  gfill p (M1 x) = M1 . gfill p x

instance Data d a => GData d (K1 i a) where
  gvals _ (K1 x) = vals x
  gfill _ (K1 x) = K1 . fill x

instance GData d U1 where
  gvals _ _   = []
  gfill _ _ _ = U1

--------------------------------------------------------------------------------

data List a = List [a] Int [a] deriving ( Eq, Ord )

list :: Int -> [a] -> List a
list n xs = List (take (n `min` 30) xs) (0 `max` (n `min` 30)) xs

instance Show a => Show (List a) where
  show (List xs n ys) = show xs {- ++ "(" ++ tail (init (show (drop n ys))) ++ ")" -}

instance Arbitrary a => Arbitrary (List a) where
  arbitrary =
    do xs <- sequence [ arbitrary | i <- [1..30] ]
       n  <- choose (0,30)
       return (list n xs)

  shrink (List _ n xs) =
    [ list k xs | k <- [0..n-1] ] ++
    [ list n (take i xs ++ [x'] ++ drop (i+1) xs)
    | i <- [0..n-1]
    , x' <- shrink (xs!!i)
    ]

instance Data d a => Data d (List a) where
  vals (List _ n xs)    = vals n ++ vals xs
  fill (List _ n xs) vs = list (fill n (take 1 vs)) (fill xs (drop 1 vs))

--------------------------------------------------------------------------------

forData :: (Show a, Data Double a) => a -> (a -> Double) -> (a, Int, Double)
forData x h = (fill x ws, n, ans)
 where
  (ws,ans,n) = goal (<= 0)
             . giveUp 10
             . take   1000
             . minimize (repeat 100) (vals x)
             $ h . fill x

--------------------------------------------------------------------------------


