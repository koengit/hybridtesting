module OptimizeNew where

import Optimize
import System.Random
import Data.Ord( comparing )
import Data.List( minimumBy, zipWith3 )

minimizeLine :: (Show a, Ord a)
             => StdGen
             -> (Double -> a)
             -> Int
             -> Double -> (Double,a) -> Double
             -> (Double,a)
minimizeLine rnd f maxTries xL (xM,aM) xR =
  go 0 rnd xL (f xL) xM aM xR (f xR)
 where
  go n rnd xL aL xM aM xR aR
    | n > maxTries || (xL==xM && xR==xM) =
      minimumBy (comparing snd) [(xL,aL),(xM,aM),(xR,aR)]
    
    -- points have a V-shape; (xM,aM) is best
    | aM <= aL && aM <= aR =
      if (xM-xL) <= (xR-xM)
        then new xM aM     x2 (f x2)
        else new x1 (f x1) xM aM

    -- points have a /-, \-, or /\-shape; (xM,aM) is not best
    | aL < aM =
       goPro (a1 < aL) xL aL x1 a1 xM aM

    | otherwise =
       goPro (a2 < aR) xM aM x2 a2 xR aR
   where
     (rnd1,rnd2) = split rnd
   
     --(x1,_) = randomR (xL,xM) rnd1
     x1 = (xL+xM)/2
     a1 = f x1

     --(x2,_) = randomR (xM,xR) rnd1
     x2 = (xM+xR)/2
     a2 = f x2

     new xA aA xB aB
       | aA <= aB  = goPro (aA < aM) xL aL xA aA xB aB
       | otherwise = goPro (aB < aM) xA aA xB aB xR aR
     
     goPro False xL aL xM aM xR aR = go (n+1) rnd2 xL aL xM aM xR aR
     goPro True  xL aL xM aM xR aR = minimumBy (comparing snd) [(xL,aL),(xM,aM),(xR,aR)]
     --goPro True  xL aL xM aM xR aR = go 0     rnd2 xL aL xM aM xR aR

minimizeBox :: (Show a, Ord a)
            => StdGen
            -> ([Double] -> a)
            -> [(Double,Double)]
            -> [([Double],a)]
minimizeBox rnd f xsLR = go 0 rnd xs0 a0
 where
  xs0 = [ (xL+xR)/2 | (xL,xR) <- xsLR ]
  a0  = f xs0

  go n rnd xs a = (xs,a) : go (n+1) rnd2 (h z) a'
   where
    (rnd1,rnd2) = split rnd'
   
    -- generate a random point the box [-1,1]
    (cs,rnd') = generate rnd xsLR
     where
      generate rnd []         = ([],  rnd)
      generate rnd (xLR:xsLR) = (c:cs,rnd2)
       where
        (c, rnd1) = randomR (-1, 1 :: Double) rnd
        (cs,rnd2) = generate rnd1 xsLR

    -- minimizing over the line through xs and ys
    h z      = zipWith3 (\x c (xL,xR) -> xL `max` (xR `min` (x + z*c))) xs cs xsLR
    (z,a')   = minimizeLine rnd1 (f . h) maxTries zL (0,a) zR
    maxTries = round (10+log (1+fromIntegral n))

    -- computing reasonable bounds on z
    zL = minimum [ zL | (zL,zR) <- zsLR ]
    zR = maximum [ zR | (zL,zR) <- zsLR ]
    
    zsLR =
      [ ord (zL,zR)
      | ((x,c),(xL,xR)) <- (xs `zip` cs) `zip` xsLR
      , let (zL,zR)
              | c == 0    = (0, 1) -- special case that'll never happen, but anyway...
              | otherwise = ((xL-x)/c, (xR-x)/c)
      ]

    ord (x,y) | x <= y    = (x,y)
              | otherwise = (y,x)

