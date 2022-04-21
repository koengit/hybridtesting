module Falsify( falsifyLine, falsifyBox ) where

import System.Random
import Data.Ord( comparing )
import Data.List( minimumBy, zipWith3 )

data Segment
  = Segment
  { sIn  :: Double
  , xIn  :: Double
  , aIn  :: Double

  , sOut :: Double
  , xOut :: Double
  , aOut :: Double
  
  , s    :: Double
  
  , xNew :: Double
  , cost :: Double
  }
 deriving ( Eq, Ord, Show )

mkSegment :: Double -> (Double, Double) -> (Double, Double) -> Double -> Segment
mkSegment sIn (xIn,aIn) (xOut,aOut) sOut =
  Segment
  { sIn = sIn
  , xIn = xIn
  , aIn = aIn
  
  , sOut = sOut
  , xOut = xOut
  , aOut = aOut
  
  , s    = s
  , xNew = xNew
  , cost = cost
  }
 where
  -- internal slope
  s = (aOut-aIn) / (xOut-xIn)

  -- three possible new sample points
  xL = xOut - (aOut / sOut)
  xR = xIn  - (aIn / sIn) -- minus (-) because sIn is treated negatively
  xM = xIn  + (xOut-xIn) * sqaIn / (sqaIn + sqaOut)
   where
    sqaIn  = sqrt aIn
    sqaOut = sqrt aOut

  xNew = case (xIn < xL && xL < xOut, xIn < xR && xR < xOut) of
           (True,  False)            -> xL
           (False, True)             -> xR
           (True,  True)
             | (xL-xIn) < (xOut-xR)  -> xR
             | otherwise             -> xL
           _ | xIn < xM && xM < xOut -> xM
             | otherwise             -> (xIn+xOut) / 2 -- emergency solution


  -- delta in slope-change if we sample xNew
  cost = abs (s1 - sIn) + abs (s2 - s1) + abs (sOut - s2)
       - abs (s  - sIn)                 - abs (sOut - s)
   where
    s1 = (-aIn) / (xNew - xIn)
    s2 = aOut   / (xOut - xNew)

falsifyLine :: (Double -> Double)
            -> Double -> (Double,Double) -> Double
            -> [(Double,Double)]
falsifyLine f xL (xM,aM) xR
  | xL /= xL || xR /= xR || xM /= xM = error (show (xL,xR))
  | otherwise = takeUntil ((<0).snd) $ inits ++ go [sg0,sg1,sg2,sg3]
 where
  aL = f xL
  aR = f xR
 
  inits = [] -- [(xL,aL),(xR,aR)] -- ++ [(xM,aM)]
 
  sg0 = mkSegment 0       (xL-1,aL) (xL,  aL) (s sg1)
  sg1 = mkSegment 0       (xL,  aL) (xM,  aM) (s sg2)
  sg2 = mkSegment (s sg1) (xM,  aM) (xR,  aR) 0
  sg3 = mkSegment (s sg2) (xR,  aR) (xR+1,aR) 0
 
  go sgs = (xN,aN) : go (update sg xN aN sgs)
   where
    sg = minimumBy (comparing cost) (tail (init sgs))

    xN = xNew sg
    aN = f xN

  update sg xN aN (sg0:sg1:sg2:sgs)
    | xIn sg == xIn sg1 = sg0' : sg1L : sg1R : sg2' : sgs
    | otherwise         = sg0 : update sg xN aN (sg1:sg2:sgs)
   where
    sg0' = mkSegment (sIn sg0) (xIn sg0, aIn sg0) (xOut sg0, aOut sg0) (s sg1L)
    sg1L = mkSegment (sIn sg1) (xIn sg1, aIn sg1) (xN,       aN)       (s sg1R)
    sg1R = mkSegment (s sg1L)  (xN,      aN)      (xOut sg1, aOut sg1) (sOut sg1)
    sg2' = mkSegment (s sg1R)  (xIn sg2, aIn sg2) (xOut sg2, aOut sg2) (sOut sg2)

  update sg xN aN sgs = error (show (sg,xN,aN,sgs))

takeUntil p [] = []
takeUntil p (x:xs)
  | p x        = [x]
  | otherwise  = x : takeUntil p xs

falsifyBox :: StdGen
           -> ([Double] -> Double)
           -> [(Double,Double)]
           -> [([Double],Double)]
falsifyBox rnd f xsLR = go 0 rnd xs0 a0
 where
  xs0 = [ (xL+xR)/2 | (xL,xR) <- xsLR ]
  a0  = f xs0

  go n rnd xs a = (xs,a) : go (n+1) rnd' (h z) a'
   where
    -- generate a random point the box [-1,1]
    (cs,rnd') = generate rnd xsLR
     where
      generate rnd []         = ([],  rnd)
      generate rnd (xLR:xsLR) = (clamp c:cs,rnd2)
       where
        (c, rnd1) = randomR (-1, 1 :: Double) rnd
        (cs,rnd2) = generate rnd1 xsLR

      clamp x -- | -0.3 < x && x < 0.3 = 0
              | otherwise           = x

    -- minimizing over the line through xs and ys
    h z      = zipWith3 (\x c (xL,xR) -> xL `max` (xR `min` (x + z*c))) xs cs xsLR
    zas      = falsifyLine (f . h) zL (0,a) zR
    (z,a')   = best maxTries zas
    maxTries = round (13 + log (fromIntegral n + 1))

    best m ((z,a):zas) = go 0 z a zas
     where
      go _ z a []   = (z,a)
      go k z a ((w,b):zas)
        | b < a     = go 0 w b zas
        | k > m     = (z,a)
        | otherwise = go (k+1) z a zas

    -- computing reasonable bounds on z
    zL = maximum [ zL | (zL,zR) <- zsLR ]
    zR = minimum [ zR | (zL,zR) <- zsLR ]
    
    zsLR =
      [ ord (zL,zR)
      | ((x,c),(xL,xR)) <- (xs `zip` cs) `zip` xsLR
      , let (zL,zR)
              | c == 0    = (0, 1) -- special case that'll never happen, but anyway...
              | otherwise = ((xL-x)/c, (xR-x)/c)
      ]

    ord (x,y) | x <= y    = (x,y)
              | otherwise = (y,x)
{-
falsifyBox2 :: StdGen
            -> ([Double] -> Double)
            -> [(Double,Double)]
            -> [([Double],Double)]
falsifyBox2 rnd f xsLR = go 0 rnd xs0 a0
 where
  xs0 = [ (xL+xR)/2 | (xL,xR) <- xsLR ]
  a0  = f xs0

  go n rnd xs a = (xs,a) : go (n+1) rnd' (h z) a'
   where
    -- generate a random point the box [-1,1]
    (cs,rnd') = generate rnd xsLR
     where
      generate rnd []         = ([],  rnd)
      generate rnd (xLR:xsLR) = (clamp c:cs,rnd2)
       where
        (c, rnd1) = randomR (-1, 1 :: Double) rnd
        (cs,rnd2) = generate rnd1 xsLR

      clamp x -- | -0.3 < x && x < 0.3 = 0
              | otherwise           = x

    -- minimizing over the line through xs and ys
    h z      = zipWith3 (\x c (xL,xR) -> xL `max` (xR `min` (x + z*c))) xs cs xsLR
    zas      = falsifyLine (f . h) zL (0,a) zR
    (z,a')   = best maxTries zas
    maxTries = round (13 + log (fromIntegral n + 1))

    best m ((z,a):zas) = go 0 z a zas
     where
      go _ z a []   = (z,a)
      go k z a ((w,b):zas)
        | b < a     = go 0 w b zas
        | k > m     = (z,a)
        | otherwise = go (k+1) z a zas

    -- computing reasonable bounds on z
    zL = maximum [ zL | (zL,zR) <- zsLR ]
    zR = minimum [ zR | (zL,zR) <- zsLR ]
    
    zsLR =
      [ ord (zL,zR)
      | ((x,c),(xL,xR)) <- (xs `zip` cs) `zip` xsLR
      , let (zL,zR)
              | c == 0    = (0, 1) -- special case that'll never happen, but anyway...
              | otherwise = ((xL-x)/c, (xR-x)/c)
      ]

    ord (x,y) | x <= y    = (x,y)
              | otherwise = (y,x)

falsifyBox2 :: StdGen
            -> ([Double] -> Double)
            -> [(Double,Double)]
            -> [([Double],Double)]
falsifyBox2 rnd f xsLR = go rnd []
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
      | out > 0                       = (2, out)
      | map fst (take 1 dists) == [0] = (3, 0)
      | otherwise                     =
        (1, sum [ abs (s - s')
                | (d,(xs,y)) <- take 2 dists
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
-}

