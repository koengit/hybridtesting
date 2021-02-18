{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Dist where

import Data.List( insert, sort, sortBy, group )
import qualified Data.Set as S
import Data.Ord( comparing )
import Control.Monad( guard )
import Graphics.EasyPlot

data Dist
  = Dist
  { val  :: Double
  , dist :: [Segment] -- strictly ordered
  }
 deriving ( Eq, Ord, Show )

data Segment
  = Segment{ line :: Line, interval :: Interval } -- closed, >=0
 deriving ( Eq, Show )

data Line
  = Line{ slope :: Double, offset :: Double }
 deriving ( Eq, Show )

type Interval = (Double, Double)
type Point = (Double, Double)

start, end :: Segment -> Point
start Segment{interval = (x, _), line = l} = (x, l `at` x)
end Segment{interval = (_, x), line = l} = (x, l `at` x)

-- Points are lines segments over an interval of length 0
pattern Point p <- (matchPoint -> Just p) where
  Point (x, y) =
    Segment{
      line = Line{slope = 0, offset = y},
      interval = (x, x) }

matchPoint :: Segment -> Maybe Point
matchPoint s
  | isPoint s = Just (start s)
  | otherwise = Nothing

isPoint :: Segment -> Bool
isPoint s = fst (interval s) == snd (interval s)

-- Returns Nothing in case both points have the same (or too close
-- for floating point) x-value
lineThrough :: Point -> Point -> Maybe Line
lineThrough (x1, y1) (x2, y2)
  | isInfinite slope || isNaN slope || isInfinite offset = Nothing
  | otherwise = Just Line{ slope = slope, offset = offset }
  where
    slope = (y2-y1)/(x2-x1)
    offset = y1 - slope*x1

intervalBetween :: Double -> Double -> Interval
intervalBetween x y = (min x y, max x y)

segment :: Point -> Point -> Segment
segment p q =
  case lineThrough p q of
    Just l -> Segment{ line = l, interval = intervalBetween (fst p) (fst q) }
    Nothing -> Point (fst p, min (snd p) (snd q))

segments :: [Point] -> [Segment]
segments [] = []
segments [p] = [Point p]
segments ps =
  simplify (zipWith segment ps (tail ps))

simplify :: [Segment] -> [Segment]
simplify = simp
  where
    simp (Point p:Point q:ss)
      | p `above` q = simp (Point q:ss)
      | q `above` p = simp (Point p:ss)
    simp (Point p:s@Segment{}:ss)
      | p `above` start s = simp (s:ss)
    simp (s@Segment{}:Point p:ss)
      | p `above` end s = simp (s:ss)
    simp (s:ss) = s:simp ss
    simp [] = []

    (x1, y1) `above` (x2, y2) =
      x1 == x2 && y1 >= y2

combineEndpoints :: (Double -> Double -> Double) -> Segment -> Segment -> [Segment]
combineEndpoints f s@Segment{} t@Segment{} =
  nub [ segment (g x) (g y) | (x, y) <- pairs ]
  where
    g ((x1, d1), (x2, d2)) = (f x1 x2, d1 + d2)
    pairs = nub
      [((p, q1), (p, q2)) | p <- [p1, p2]] ++
      [((p1, q), (p2, q)) | q <- [q1, q2]]
    p1 = start s
    p2 = end s
    q1 = start t
    q2 = end t

distAdd :: Dist -> Dist -> Dist
distAdd = lift2 (combineEndpoints (+))

distMul :: Dist -> Dist -> Dist
distMul = lift2 pieceMul
  where
    pieceMul p q =
      squareRootSegment p q ++
      combineEndpoints (*) p q

    squareRootSegment s1@Segment{line = Line{slope = a}} s2@Segment{line = Line{slope = b}}
      | not (isPoint s1) && not (isPoint s2) && a /= 0 && b /= 0 =
        map (scale (1/(a*b))) (squareRootSegment' (scale a s1) (scale b s2))
    squareRootSegment _ _ = []

    squareRootSegment' s1 s2
      | 0 <= z1 && z1 < z2 =
        map (mapDistance (\d -> 2*(d-z1) + line s1 `at` z1 + line s2 `at` z1)) (squareRootApprox z1 z2)
      | otherwise = []
      where
        z1 = fst (interval s1) `max` fst (interval s2)
        z2 = snd (interval s1) `min` snd (interval s2)

    -- approximate sqrt function between a^2 and c^2
    squareRootApprox :: Double -> Double -> [Segment]
    squareRootApprox a c =
      segments [(a^2, a), (b^2, b), (c^2, c)]
      where
        b = (a+c)/2 -- minimises absolute error

scale :: Double -> Segment -> Segment
scale x = mapValue (* x)

-- f must be linear
mapValue :: (Double -> Double) -> Segment -> Segment
mapValue f (Point (x, d)) = Point (f x, d)
mapValue f s@Segment{interval = (x, y)} =
  segment (f x1, d1) (f x2, d2)
  where
    (x1, d1) = start s
    (x2, d2) = end s

-- f must be linear
mapDistance :: (Double -> Double) -> Segment -> Segment
mapDistance f (Point (x, d)) = Point (x, f d)
mapDistance f s@Segment{line = l} =
  s{line = l{offset = f0, slope = f1 - f0}}
  where
    f0 = f (offset l)
    f1 = f (offset l + slope l)

instance Ord Segment where
  compare = comparing tuple
   where
    tuple p = (start p, mslope p, -snd (interval p))

constant :: Double -> Dist
constant x = Dist x [Point (x,0)]

input :: (Double,Double) -> Double -> Dist
input (a,b) x
 | a <= x && x <= b = Dist x $ segments [(a,x-a), (x,0), (b,b-x)]
 | otherwise        = error "input out of bounds"

lift2 :: (Segment -> Segment -> [Segment]) -> Dist -> Dist -> Dist
lift2 f a b =
  Dist
  { val  = f0 (val a) (val b)
  , dist = simplify $ sort [ r | p <- dist a, q <- dist b, r <- f p q ]
  }
 where
  f0 x y = let (Point (z,_):_) = f (Point (x,0)) (Point (y,0)) in z
{-
norm :: [Piece] -> [Piece]
norm ps = linesAndPoints (twoPoints (usort ps))
 where
  -- remove Points that are on the same x
  twoPoints (p@(Point (x1,_)) : Point (x2,_) : ps)
    | x1 == x2       = twoPoints (p : ps)
  twoPoints (p : ps) = p : twoPoints ps
  twoPoints []       = []

  -- a single Point at the beginning does not interfere with anything
  linesAndPoints (p@(Point _) : ps) =
    p : linesAndPoints ps

  -- a Line and something that lies beyond the Line: commit to the Line
  linesAndPoints (l@(Line _ (x2,_)) : p : ps)
    | value (start p) >= x2 =
      l : linesAndPoints (p : ps)
  
  -- a Line and a Point: break the line if necessary
  linesAndPoints (l@(Line (x1,a1) (x2,a2)) : p@(Point (x,a)) : ps)
    | a' <= a =
      linesAndPoints (l : ps)

    | otherwise =
      Line (x1,a1) (x,a') : p : linesAndPoints (insert (Line (x,a') (x2,a2)) ps)
   where
    a' = l `at` x

  -- two Lines with the second Line starting somewhere below/on(-downwards) the first
  linesAndPoints (l@(Line (x1,a1) (x2,a2)) : m@(Line (y1,b1) (y2,b2)) : ps)
    | b1 < a' || x1 < y1 && b1 == a' && (m `at` x2 < a2) =
      Line (x1,a1) (y1,a') : Point (y1,a') :
        linesAndPoints (m : insert (Line (y1,a') (x2,a2)) ps)
   where
    a' = l `at` y1
  
  -- two Lines with the second Line starting somewhere above/on(-upwards) the first
  linesAndPoints (l@(Line (x1,a1) (x2,a2)) : m@(Line (y1,b1) (y2,b2)) : ps) =
    case l `cross` m of
      -- if they cross, cut off the bit above the first Line from the second
      Just x ->
        linesAndPoints (l : insert (Line (x,a) (y2,b2)) ps) 
       where
        a = l `at` x  
  
      -- if they don't cross, maybe keep the bit of the second Line that is longer than the first
      Nothing
        | y2 <= x2  -> linesAndPoints (l : ps) 
        | otherwise -> linesAndPoints (l : insert (Line (x2,a) (y2,b2)) ps)
       where
        a = m `at` x2

  linesAndPoints [p] = [p]
  linesAndPoints [] = []
      
  Line (x1,a1) (x2,a2) `cross` Line (y1,b1) (y2,b2)
    | s1 == s2                             = Nothing
    | x1 < x && x < x2 && y1 < x && x < y2 = Just x
    | otherwise                            = Nothing
   where
    s1 = (a2-a1) / (x2-x1)
    s2 = (b2-b1) / (y2-y1)
    x  = (s2*y1 - s1*x1 + a1 - b1) / (s2-s1)  
-}

at :: Line -> Double -> Double
l `at` x = slope l * x + offset l

mslope :: Segment -> Maybe Double
mslope (Point _) = Nothing
mslope Segment{line = l} = Just (slope l)

--

nub :: Ord a => [a] -> [a]
nub xs = go S.empty xs
 where
  go seen (x:xs)
    | x `S.member` seen = go seen xs
    | otherwise         = x : go (S.insert x seen) xs
  go _ []               = []

usort :: Ord a => [a] -> [a]
usort = map head . group . sort

--

plotDist :: Dist -> IO Bool
plotDist d =
  plot X11 $ map plotSeg (dist d)
  where
    plotSeg (Point (x, d)) =
      Data2D [] [] [(x, d)]
    plotSeg Segment{line = l, interval = (x, y)} =
      Function2D [] [Range x y] (\x -> slope l*x + offset l)

showBool :: Dist -> String
showBool d =
  show (val d == 1) ++ " with robustness " ++
  case lookup (1 - val d) [p | Point p <- simplify (dist d)] of
    Nothing -> "infinity"
    Just y  -> show y
