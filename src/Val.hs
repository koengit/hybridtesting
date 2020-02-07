{-# LANGUAGE DeriveGeneric #-}
module Val where

import qualified Data.Map as M
import Data.List( sort, sortBy, intercalate )
import Data.Ord
import System.Process( system )
import Debug.Trace
import Utils

--------------------------------------------------------------------------------
-- Val keeps track of a value, plus alternatives

newtype Val a = Val{ vals :: [(a,Double{- >=0 -})] }
 deriving ( Eq, Ord, Show )

the :: Val a -> a
the = fst . head . vals

val :: a -> Val a
val x = Val [(x, 0)]

mkVal :: Ord a => [(a,Double)] -> Val a
mkVal xs = Val (ordNubOn fst xs)

forget :: Int -> Val a -> Val a
forget n (Val xs) = Val (take n xs)

share :: Val a -> Val (Val a)
share (Val xs) = Val [(Val [x], 0) | x <- xs]

merge :: [(a, Double)] -> [(a, Double)] -> [(a, Double)]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys)
  | snd x <= snd y = x:merge xs (y:ys)
  | otherwise = y:merge (x:xs) ys

mergeSorted :: [[(a, Double)]] -> [(a, Double)]
mergeSorted [] = []
mergeSorted [xs] = xs
mergeSorted ([]:ys:xss) = mergeSorted (ys:xss)
mergeSorted ((x:x':xs):(y:ys):xss)
  | snd x' > snd y = x:mergeSorted (merge (x':xs) (y:ys):xss)
mergeSorted ((x:xs):ys:xss) =
  x:mergeSorted (xs:ys:xss)

--------------------------------------------------------------------------------
-- basic lifting

mapVal :: Ord b => (a->b) -> Val a -> Val b
mapVal f (Val xs) =
  mkVal
  [ (f x, a)
  | (x,a) <- xs
  ]

liftVal :: Ord c => (a->b->c) -> Val a -> Val b -> Val c
liftVal f (Val xs) (Val ys) =
  mkVal $ mergeSorted $
    [ [ (f x y, a + b)
      | (y,b) <- ys ]
    | (x,a) <- xs
    ]

smashVal :: Ord a => Val (Val a) -> Val a -- monadic join
smashVal (Val vs) =
  mkVal $ mergeSorted $
    [ [ (z, a + b)
      | (z,b) <- zs ]
    | (Val zs,a) <- vs
    ]

bindVal :: Ord b => Val a -> (a -> Val b) -> Val b
bindVal x f = smashVal (mapVal f x)

--------------------------------------------------------------------------------
-- basic operations

(||?), (&&?), (=>?) :: Val Bool -> Val Bool -> Val Bool
(||?) = liftVal (||)
(=>?) = liftVal (<=) -- this is correct: F<=F, F<=T, T<=T but not(T<=F)
(&&?) = liftVal (&&)
nott  = mapVal not

(==>?) :: Val Bool -> Val Bool -> Val Bool
a ==>? b | the a     = b
         | otherwise = Val [ (b, if b then v else v+99999) | (b,v) <- vals a ]

instance (Ord a, Num a) => Num (Val a) where
  (+)         = liftVal (+)
  (-)         = liftVal (-)
  (*)         = liftVal (*)
  abs         = mapVal abs
  negate      = mapVal negate
  signum      = mapVal signum
  fromInteger = val . fromInteger

instance (Ord a, Fractional a) => Fractional (Val a) where
  (/)          = liftVal (/)
  recip        = mapVal recip
  fromRational = val . fromRational

instance (Ord a, Floating a) => Floating (Val a) where
  pi    = val pi
  exp   = mapVal exp
  log   = mapVal log
  sin   = mapVal sin
  cos   = mapVal cos
  asin  = mapVal asin
  acos  = mapVal acos
  atan  = mapVal atan
  sinh  = mapVal sinh
  cosh  = mapVal cosh
  asinh = mapVal asinh
  acosh = mapVal acosh
  atanh = mapVal atanh

class VCompare a where
  (==?), (/=?), (>?), (>=?), (<?), (<=?) :: a -> a -> Val Bool

  x /=? y = mapVal not (x ==? y)
  x >=? y = y <=? x
  x >?  y = y <? x
  x <?  y = mapVal not (y <=? x)

instance VCompare Double where
  x ==? y = eqZero (y - x)
  x <=? y = geqZero (y - x)

eqZero :: Double -> Val Bool
eqZero x
  | x == 0    = Val [(True,0)]
  | otherwise = Val [(False,0)]

geqZero :: Double -> Val Bool
geqZero x
  | x >= 0    = Val [(True,0),  (False,x)]
  | otherwise = Val [(False,0), (True,-x)]

instance VCompare a => VCompare (Val a) where
  (==?) = compVal (==?)
  (/=?) = compVal (/=?)
  (>?)  = compVal (>?)
  (>=?) = compVal (>=?)
  (<?)  = compVal (<?)
  (<=?) = compVal (<=?)

compVal op x y =
  smashVal (liftVal op x y)

--------------------------------------------------------------------------------

class Choice a where
  ifThenElse :: Val Bool -> a -> a -> a

instance Choice Double where
  ifThenElse c x y =
    if the c then x else y -- loses distance info

instance Ord a => Choice (Val a) where
  ifThenElse (Val cs) (Val xs) (Val ys) =
    mkVal $ mergeSorted $
    [ [ (z,dc + dz)
      | (z,dz) <- if c then xs else ys ]
    | (c,dc) <- cs
    ]

--------------------------------------------------------------------------------

howTrue :: Val Bool -> Double
howTrue (Val [(True,0)])             = infinity
howTrue (Val ((True,0):(False,d):_)) = d + 1
howTrue (Val [(False,0)])            = -infinity
howTrue (Val ((False,0):(True,d):_)) = -d - 1

falsify :: Val Bool -> Maybe Double
falsify (Val [(True,0)])             = Just infinity
falsify (Val ((True,0):(False,d):_)) = Just d
falsify _                            = Nothing

infinity :: Double
infinity = 1/0

--------------------------------------------------------------------------------
