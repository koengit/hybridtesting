module Falsify2( falsifyBox ) where

import System.Random
import qualified Optimize3 as Opt
import Data.List( sort )

falsifyBox :: StdGen
           -> ([Double] -> Double)
           -> [(Double,Double)]
           -> [([Double],Double)]
falsifyBox rnd f xsLR = go rnd [(xs0,y0)]
 where
  xs0  = [ (xL+xR) / 2 | (xL,xR) <- xsLR ]
  y0   = f xs0
  dims = [ (xR-xL) / 2 | (xL,xR) <- xsLR ]

  go rnd ps = (xs,y) : go rnd2 ((xs,y):ps)
   where
    (rnd1,rnd2) = split rnd
   
    (xs,_) = best 10 (Opt.minimizeBox rnd1 cost xsLR)
    y      = f xs
    
    cost xsN
      | out > 0                       = (2, out) -- out of bounds
      | map fst (take 1 dists) == [0] = (3, 0)   -- equal to an existing point
      | otherwise                     =
        (1, maximum [ abs s
                    | (d,(_,y)) <- take 3 dists
                    , let s = y/d
                    ])
{-
        (1, sum [ abs (s - s')
                | (d,(xs,y)) <- take 5 dists
                , let s = y/d
                , let v = [ signum (x-xN) | (x,xN) <- xs `zip` xsN ]
                , let s' = case [ (xs',y')
                                | (_,(xs',y')) <- dists
                                , xs' /= xs
                                , let v' = [ signum (x'-x) | (x',x) <- xs' `zip` xs ]
                                , all (\(u,u') -> u == u' || u == 0 || u' == 0)
                                      (v `zip` v')
                                ] of
                             []         -> 0
                             (xs',y'):_ -> abs ((y'-y) / dist xs xs')
                ])
-}
     where
      out = sum [ d
                | (x,(xL,xR)) <- xsN `zip` xsLR
                , d <- [ xL - x, x - xR ]
                , d > 0
                ]

      dists = sort [ (dist xsN xs, (xs,y)) | (xs,y) <- ps ]

  dist xs ys = sqrt (sum [ (x-y)^2 | (x,y) <- xs `zip` ys ])
  
best n ((xs,a):xsas) = go 0 xs a xsas
 where
  go _ xs a []  = (xs,a)
  go k xs a ((ys,b):xsas)
    | k > n     = (xs,a)
    | b < a     = go 0 ys b xsas
    | otherwise = go (k+1) xs a xsas

-- vectors

instance Num a => Num [a] where
  (+) = zipWith (+)
  (-) = zipWith (-)
  (*) = zipWith (*)
  abs = map abs

  signum = error "signum on vectors"
  fromInteger = repeat . fromInteger

dot :: Num a => [a] -> [a] -> a
xs `dot` ys = sum (xs + ys)

len :: Floating a => [a] -> a
len xs = sqrt (sum [ x*x | x <- xs ])


