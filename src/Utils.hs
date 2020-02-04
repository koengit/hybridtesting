module Utils where

import Control.Arrow((&&&))
import Data.List(groupBy, sortBy)
import Data.Ord(comparing)
import qualified Data.Set as Set

partitionBy :: Ord b => (a -> b) -> [a] -> [[a]]
partitionBy value =
  map (map fst) .
  groupBy (\x y -> snd x == snd y) .
  sortBy (comparing snd) .
  map (id &&& value)

collate :: Ord a => ([b] -> c) -> [(a, b)] -> [(a, c)]
collate f = map g . partitionBy fst
  where
    g xs = (fst (head xs), f (map snd xs))

isSorted :: Ord a => [a] -> Bool
isSorted xs = and (zipWith (<=) xs (tail xs))

isSortedBy :: Ord b => (a -> b) -> [a] -> Bool
isSortedBy f xs = isSorted (map f xs)

ordNub :: Ord a => [a] -> [a]
ordNub = ordNubOn id

ordNubOn :: Ord b => (a -> b) -> [a] -> [a]
ordNubOn f = from Set.empty
  where
    from _ [] = []
    from s (x:xs)
      | f x `Set.member` s = from s xs
      | otherwise = x:from (Set.insert (f x) s) xs

usort :: Ord a => [a] -> [a]
usort = usortBy compare

usortBy :: (a -> a -> Ordering) -> [a] -> [a]
usortBy f = map head . groupBy (\x y -> f x y == EQ) . sortBy f

sortBy' :: Ord b => (a -> b) -> [a] -> [a]
sortBy' f = map snd . sortBy (comparing fst) . map (\x -> (f x, x))

usortBy' :: Ord b => (a -> b) -> [a] -> [a]
usortBy' f = map snd . usortBy (comparing fst) . map (\x -> (f x, x))

{-# INLINE fixpoint #-}
fixpoint :: Eq a => (a -> a) -> a -> a
fixpoint f x = fxp x
  where
    fxp x
      | x == y = x
      | otherwise = fxp y
      where
        y = f x

foldn :: Int -> (a -> a) -> a -> a
foldn 0 _ x = x
foldn n f x = f (foldn (n-1) f x)

takeUntil :: (a -> Bool) -> [a] -> [a]
takeUntil p (x:xs)
  | p x        = [x]
  | otherwise  = x : takeUntil p xs
takeUntil p [] = []

