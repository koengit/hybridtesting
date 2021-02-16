
import qualified Data.Map as M
import Control.Monad.Identity
import Process.Plot
import Process.Language( Var(..) )
import Process.Eval( Value(..) )
import Val
import OptimizeNew as O1
import Optimize as O0
import Optimize2 as O2
import Optimize3 as O3
import System.Random
import Data.List( transpose, sort )
--import Falsify2( falsifyBox )
import Falsify( falsifyBox )
import qualified Falsify2 as F2

{-
f x =
  ifThenElse ((x <? 99.8) ||? (x >? 100.2))
    (abs (100-abs (100-x)))
    (100-x)

main1 = plot "example" delta
       [ M.fromList
         [ (Global "f", DoubleValue `mapVal` (f (val x)))
         , (Global "fT", BoolValue `mapVal` (f (val x) <? 101))
         ]
       | x <- [0,delta..250]
       ]
 where
  delta = 0.1
-}

alpine1 xs =
  sum [ abs (x * sin x + 0.1*x + 0.1) | x <- xs ]

booth [x,y] = (x + 2*y - 7)^2 + (2*x - y - 5)^2

vetters [x,y,z] = (1 / (1 + (x - y)^2)) + sin ((pi * y + z) / 2) + exp ((((x+y)/y)-2)^2)

wolfe [x,y,z] =
  (4/3)*(x*x + y*y - x*y)**0.75 + z

happycat xs =
  ((lxs2 - n)^2)**a + (lxs2/2 + sum xs)/n + 0.5
 where
  n = fromIntegral (length xs)
  a = 1/8
  lxs2 = sum [ x*x | x <- xs ]

koen xs =
  sum [ abs $ if even (round (x+y)) then x-5*cos y else x-4*sin y
      | (x,y) <- xs `zip` tail xs
      ]

sines [x] =
  (sin (x/(5+cos x)) + 2.0 - 1 / (2.935 + 1.2*cos (2.5*x) + abs (0.01 - (x/10000))) + 0.5*cos x)

main =
  do sequence_
       [ putStrLn (show i ++ ": " ++ show y ++ " " ++ show (sh x))
       | (i,(x,y)) <- progress ([1..] `zip` xys')
       ]
 where
  delta = 0.1
{-
  k = 5
  n = k*k
  f = magic2 k
  (aL,aR) = (0,100) -- (0.5001,fromIntegral n + 0.4999)
  sh xs = map snd (sort (xs `zip` [1..]))
  p (xs,y) = y <= 0.1
-}
{-
  k = 4
  n = k*k
  f = magic k
  (aL,aR) = (0.5001,fromIntegral n + 0.4999)
  sh xs = map round xs
  p (xs,y) = y <= 0.1
-}
{-
  n = 14
  f = koen
  (aL,aR) = (-10,10)
  sh = map (\x -> fromIntegral (round (x*10)) / 10)
  p (xs,y) = y <= 0.1
-}
{-
  n = 1
  f = sines -- alpine1
  (aL,aR) = (0,300)
  sh x = fromIntegral (round (x*1000)) / 1000
  p (xs,y) = y <= 0.01
-}
-- {-
  n = 5
  f = alpine1
  (aL,aR) = (-5,5)
  sh = map $ \x -> fromIntegral (round (x*10)) / 10
  p (xs,y) = y <= 0.01
-- -}
{-
  n = 3
  f = wolfe
  (aL,aR) = (-1,2)
  sh = map $ \x -> fromIntegral (round (x*10)) / 10
  p (xs,y) = y <= 0.001
-}
{-
  n = 2
  f = happycat
  (aL,aR) = (-2,2)
  sh = map $ \x -> fromIntegral (round (x*100)) / 100
  p (xs,y) = y <= 0.01
-}
  xys' = takeUntil p xys

  xys :: [([Double],Double)]  
  --xys = [ (x,y) | (x,y,_) <- minimize (replicate n 10) (replicate n 0) f ]
  --xys = O1.minimizeBox (mkStdGen 11) f [(aL,aR) | i <- [1..n]]
  --xys = O2.minimizeBox (mkStdGen 114) f [(aL,aR) | i <- [1..n]]
  --xys = O3.minimizeBox (mkStdGen 12) f [(aL,aR) | i <- [1..n]]
  xys = falsifyBox (mkStdGen 1111111) f [(aL,aR) | i <- [1..n]]
  --xys = F2.falsifyBox (mkStdGen 11111) f [(aL,aR) | i <- [1..n]]
  --xys = [ ([x],y) | (x,y) <- falsifyLine (\x -> f (replicate n x)) aL (0,1) aR ]
  --xys = O3.minimizeBox (mkStdGen 114) (\[x] -> f (replicate n x)) [(aL,aR)]

progress ((i,(xs,a)):xsas) = (i,(xs,a)) : go a xsas
 where
  go a ((i,(ys,b)):xsas)
    | b < a     = (i,(ys,b)) : go b xsas
    | otherwise = go a xsas
  go _ _ = []

takeUntil p [] = []
takeUntil p (x:xs)
  | p x        = [x]
  | otherwise  = x : takeUntil p xs

magic :: Int -> [Double] -> Double
magic k xs =
  sum $ map abs $
-- {-
  [ fromIntegral k * ((unstab x `min` unstab y) + 0.1)
  | (i,x) <- [0..] `zip` xs
  , j <- [0..i-1]
  , let y = xs!!j
  , round x == round y
  ] ++
-- -}
{-
  [ (2*minimum [ abs (fromIntegral i - x) | x <- xs ])^2
  | i <- [1..k*k]
  ] ++
-}
  concat
  [ [ sum r - tot, sum (map (fromIntegral . round) r) - tot ]
  | r <- mat ++ transpose mat ++ [diag1, diag2]
  ]
 where
  mat = takeWhile (not . null) . map (take k) . iterate (drop k) $ xs
  tot = fromIntegral (sum [ 1 .. k*k ] `div` k)

  diag1 = [ (mat!!i)!!i | i <- [0..k-1] ]
  diag2 = [ (mat!!i)!!(k-1-i) | i <- [0..k-1] ]

  unstab x = abs (x - (a + 0.5)) `min` abs (x - (a - 0.5))
   where
    a = fromIntegral (round x)
    
magic2 :: Int -> [Double] -> Double
magic2 k xs =
  sum $ map abs $
  [ fromIntegral (abs (tot-s)) +
    minimum
    [ abs (x-y)
    | a <- r
    , (x,a') <- xas
    , a' == a
    , (y,b) <- xas
    , if tot > s then b == a+1 else a == b+1
    ]
  | r <- mat ++ transpose mat ++ [diag1, diag2]
  , let s = sum r
  , s /= tot
  ]
 where
  norm xs
    | a /= 0    = [ (x - mn)/a | x <- xs ]
    | otherwise = xs
  mn  = minimum xs
  mx  = maximum xs
  a   = (mx-mn)/100
  xas = xs `zip` [1..]
  ads = [ (a,x) | (x,a) <- sort xas ]

  mat = takeWhile (not . null) . map (take k) . iterate (drop k) $ map fst ads
  tot = sum [ 1 .. k*k ] `div` k

  diag1 = [ (mat!!i)!!i | i <- [0..k-1] ]
  diag2 = [ (mat!!i)!!(k-1-i) | i <- [0..k-1] ]

