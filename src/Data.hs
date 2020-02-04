{-# LANGUAGE FlexibleContexts, DefaultSignatures, TypeOperators, MultiParamTypeClasses, FlexibleInstances #-}
module Data where

{-
Varying the data-part of test data by means of expressing them as numerical data.
-}

import Test.QuickCheck
import Data.List
import Optimize
import GHC.Generics
import qualified Data.Map as M

--------------------------------------------------------------------------------

class Data a where
  vals :: a -> [Double]
  fill :: a -> [Double] -> a

  default vals :: (Generic a, GData (Rep a)) => a -> [Double]
  vals = genericVals

  default fill :: (Generic a, GData (Rep a)) => a -> [Double] -> a
  fill = genericFill

instance Data ()
instance (Data a, Data b) => Data (a,b)
instance (Data a, Data b) => Data (Either a b)
instance Data a => Data [a]

instance Data Bool

instance Data Double where
  vals x       = [x]
  fill _ (v:_) = v

instance Data Int where
  vals n       = [fromIntegral n]
  fill _ (v:_) = round v

instance (Ord a, Data b) => Data (M.Map a b) where
  vals m    = [ x | (_,y) <- M.toList m, x <- vals y ]
  fill m vs = M.fromList (xs `zip` fill ys vs)
   where
    (xs,ys) = unzip (M.toList m)

--------------------------------------------------------------------------------

genericVals :: (Generic a, GData (Rep a)) => a -> [Double]
genericVals = gvals . from

genericFill :: (Generic a, GData (Rep a)) => a -> [Double] -> a
genericFill x xs = to (gfill (from x) xs)

class GData f where
  gvals :: f a -> [Double]
  gfill :: f a -> [Double] -> f a

instance (GData f, GData g) => GData (f :*: g) where
  gvals (x :*: y)    = gvals x ++ gvals y
  gfill (x :*: y) vs = gfill x (take k vs) :*: gfill y (drop k vs)
   where
    k = length (gvals x)

instance (GData d f, GData d g) => GData d (f :+: g) where
   gvals p (L1 x)  = gvals p x
   gvals p (R1 y) = gvals p y

   gfill (L1 x) = L1 . gfill x
   gfill (R1 y) = R1 . gfill y

instance GData f => GData (M1 i c f) where
  gvals (M1 x) = gvals x
  gfill (M1 x) = M1 . gfill x

instance Data a => GData (K1 i a) where
  gvals (K1 x) = vals x
  gfill (K1 x) = K1 . fill x

instance GData U1 where
  gvals _   = []
  gfill _ _ = U1

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

instance Data a => Data (List a) where
  vals (List _ n xs)    = vals n ++ vals xs
  fill (List _ n xs) vs = list (fill n (take 1 vs)) (fill xs (drop 1 vs))

--------------------------------------------------------------------------------

forData :: (Show a, Data a) => a -> (a -> Double) -> (a, Int, Double)
forData x h = (fill x ws, n, ans)
 where
  (ws,ans,n) = goal (<= 0)
             . giveUp 10
             . take   1000
             . minimize (repeat 100) (vals x)
             $ h . fill x

--------------------------------------------------------------------------------


