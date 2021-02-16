module Optimize2 where

import Data.List( insertBy, sortBy )
import Data.Ord( comparing )
import System.Random

type Point = [Double]

minimizeBox :: Ord a => StdGen -> ([Double] -> a) -> [(Double,Double)] -> [([Double],a)]
minimizeBox rnd f []   = [([], f [])]
minimizeBox rnd f xsLR = go rnd 0 (pair xs0)
 where
  xs0 = [ (xL+xR)/2 | (xL,xR) <- xsLR ]

  pair xs = (xs', f xs')
   where
    xs' = [ xL `max` (xR `min` x) | (x,(xL,xR)) <- xs `zip` xsLR ]
  
  go rnd n xsa = progress (5+round (log (1+fromIntegral n)))
               . nm
               . sort
               $ xsa : [ pair xs | xs <- genPoints rnd1 (length xsLR) ]
   where
    (rnd1,rnd2) = split rnd

    progress m (t@(xs,a):xsas) = t : pr 0 xs a xsas
     where
      pr k xs a (t@(ys,b):xsas)
        | a < b     = t : pr 0 ys b xsas
        | k >= m    = t : go rnd2 (n+1) (xs,a)
        | otherwise = t : pr (k+1) xs a xsas

  nm xas =
    (x0,a0) :
    if aR < aN then
      if a0 <= aR || aR <= aE then
        -- reflect
        nm (ins qR xasI)
      else
        -- expand
        nm (ins qE xasI)
    else
      if aC < aL then
        -- contract
        nm (ins qC xasI)
      else
        -- shrink
        nm (sort (q0:[ pair (x -*-> (0.5,x0)) | (x,_) <- tail xas ]))
   where
    xasI       = init xas
    q0@(x0,a0) = head xas
    qN@(_ ,aN) = last xasI
    qL@(xL,aL) = last xas

    -- centroid
    xO = centroid (map fst xasI)

    -- reflect, expand, contract
    qR@(_,aR) = pair (xL -*-> (2,    xO))
    qE@(_,aE) = pair (xL -*-> (3,    xO))
    qC@(_,aC) = pair (xL -*-> (0.45, xO)) -- not 0.5 to avoid the same point twice

  -- sort
  ins  = insertBy (comparing snd)
  sort = sortBy (comparing snd)

  -- random points
  genPoints rnd 0 = []
  genPoints rnd n = genPoint rnd1 xsLR : genPoints rnd2 (n-1)
   where
    (rnd1,rnd2) = split rnd

  genPoint rnd []         = []
  genPoint rnd (xLR:xsLR) = x : genPoint rnd' xsLR
   where
    (x,rnd') = randomR xLR rnd

centroid :: [Point] -> Point
centroid ps = [ sum [p!!i | p <- ps] / fromIntegral l | i <- [0..l-1] ]
 where
  l = length ps

(-*->) :: Point -> (Double, Point) -> Point
p -*-> (a,q) = [ x + a*(y - x) | (x,y) <- p `zip` q ]

