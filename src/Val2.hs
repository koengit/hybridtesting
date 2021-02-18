-- Typed dists.

{-# LANGUAGE DeriveGeneric, FlexibleInstances #-}
module Val2 where

import qualified Dist as Dist
import Dist(Dist)

newtype Val a = Val{ dist :: Dist }
 deriving ( Eq, Ord, Show )

class Value a where
  toDouble :: a -> Double
  fromDouble :: Double -> a

instance Value Double where
  toDouble = id
  fromDouble = id

instance Value Bool where
  toDouble False = 0
  toDouble True = 1
  fromDouble 0 = False
  fromDouble 1 = True

the :: Value a => Val a -> a
the = fromDouble . Dist.val . dist

constant :: Value a => a -> Val a
constant = Val . Dist.constant . toDouble

input :: (Double, Double) -> Double -> Val Double
input range x = Val (Dist.input range x)

mapDist :: (Dist -> Dist) -> (Val a -> Val b)
mapDist f (Val x) = Val (f x)

liftDist :: (Dist -> Dist -> Dist) -> (Val a -> Val b -> Val c)
liftDist f (Val x) (Val y) = Val (f x y)

(||?), (&&?), (=>?) :: Val Bool -> Val Bool -> Val Bool
(||?) = liftDist (Dist.||?)
(=>?) = liftDist (Dist.=>?)
(&&?) = liftDist (Dist.&&?)
nott  = mapDist Dist.nott

instance Num (Val Double) where
  (+)         = liftDist (+)
  (-)         = liftDist (-)
  (*)         = liftDist (*)
  abs         = mapDist abs
  negate      = mapDist negate
  signum      = mapDist signum
  fromInteger = constant . fromInteger

instance Fractional (Val Double) where
  (/)          = liftDist (/)
  recip        = mapDist recip
  fromRational = constant . fromRational

(==?), (/=?), (>?), (>=?), (<?), (<=?) :: Value a => Val a -> Val a -> Val Bool
(==?) = liftDist (Dist.==?)
(/=?) = liftDist (Dist./=?)
(>?)  = liftDist (Dist.>?)
(>=?) = liftDist (Dist.>=?)
(<?)  = liftDist (Dist.<?)
(<=?) = liftDist (Dist.<=?)

ifThenElse :: Val Bool -> Val a -> Val a -> Val a
ifThenElse (Val x) (Val y) (Val z) = Val (Dist.ifThenElse x y z)

howTrue :: Val Bool -> Double
howTrue = Dist.howTrue . dist

falsify :: Val Bool -> Maybe Double
falsify val =
  case falsify' val of
    Left x -> Just x
    Right _ -> Nothing

falsify' :: Val Bool -> Either Double Double
falsify' val
  | truth > 0 = Left truth
  | otherwise = Right (-truth)
  where
    truth = howTrue val
