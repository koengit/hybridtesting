{-# LANGUAGE PatternSynonyms, ViewPatterns #-}
module Dist where

import Utils
import Data.List( insert, sort, sortBy, group )
import qualified Data.Set as S
import Data.Ord( comparing )
import Control.Monad( guard )
import Graphics.EasyPlot
import Control.Monad

----------------------------------------------------------------------
-- Data types

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

----------------------------------------------------------------------
-- Computing with lines and segments

-- Evaluate a line at a given x-value
at :: Line -> Double -> Double
l `at` x = slope l * x + offset l

-- Construct a line passing through two points.
-- Returns Nothing in case both points have the same (or too close
-- for floating point) x-value.
lineThrough :: Point -> Point -> Maybe Line
lineThrough (x1, y1) (x2, y2)
  | isInfinite slope || isNaN slope || isInfinite offset = Nothing
  | otherwise = Just Line{ slope = slope, offset = offset }
  where
    slope = (y2-y1)/(x2-x1)
    offset = y1 - slope*x1

-- Find the start and end point (value and distance) of a segment
start, end :: Segment -> Point
start Segment{interval = (x, _), line = l} = (x, l `at` x)
end Segment{interval = (_, x), line = l} = (x, l `at` x)

-- Find the smallest and largest distance over the entirety of a segment
minDistance, maxDistance :: Segment -> Double
minDistance s = snd (start s) `min` snd (end s)
maxDistance s = snd (start s) `max` snd (end s)

-- Connect two points with a line segment.
-- The two points can be given in either order, and may even have
-- the same x-value (in which case the returned segment is a single Point).
segment :: Point -> Point -> Segment
segment p q =
  case lineThrough p q of
    Just l -> Segment{ line = l, interval = intervalBetween (fst p) (fst q) }
    Nothing -> Point (fst p, min (snd p) (snd q))
  where
    intervalBetween x y = (min x y, max x y)

-- Connect a list of points with line segments.
-- The points must be sorted by x-value, but duplicate x-values are allowed.
segments :: [Point] -> [Segment]
segments [] = []
segments [p] = [Point p]
segments ps =
  simplify (zipWith segment ps (tail ps))
  where
    simplify (Point p:Point q:ss)
      | p `above` q = simplify (Point q:ss)
      | q `above` p = simplify (Point p:ss)
    simplify (Point p:s@Segment{}:ss)
      | p `above` start s = simplify (s:ss)
    simplify (s@Segment{}:Point p:ss)
      | p `above` end s = simplify (s:ss)
    simplify (s:ss) = s:simplify ss
    simplify [] = []

    (x1, y1) `above` (x2, y2) =
      x1 == x2 && y1 >= y2

instance Ord Segment where
  compare = comparing tuple
   where
    tuple p = (start p, mslope p)
    mslope (Point _) = Nothing
    mslope Segment{line = l} = Just (slope l)

-- Given a list of segments in any order, possibly overlapping,
-- transform them to be strictly ordered and non-overlapping
norm :: [Segment] -> [Segment]
norm vs = glue (simp (usort vs))
 where
  glue (v : w : vs) | line v == line w && xv2 >= xw1 =
    glue (v{ interval = (xv1, xv2 `max` xw2) } : vs)
   where
    xv1 = fst (interval v)
    xv2 = snd (interval v)
    xw1 = fst (interval w)
    xw2 = snd (interval w)
 
  glue (v : vs) = v : glue vs
  glue []       = []
 
  simp (v : w : vs)
    -- if v ends before w starts: commit v
    | xv2 < xw1 =
        v : simp (w : vs)
        
    -- if v starts before w starts: split v
    | xv1 < xw1 =
        v{ interval = (xv1,xw1) } : simp (insert v{ interval = (xw1,xv2) } (w:vs))

    -- ... (now, v and w have the same starting point; w will start above= v)
    
    -- if w's cutoff point lies beyond w: delete w
    | xw2 <= xwc =
        simp (v : vs)
    
    -- if w's cutoff point would make w into a point: commit v if, but only if it's lower than w
    | xwc <= xw1 =
        [ v | yv1 < yw1 ] ++ simp (w : vs)
    
    -- otherwise: cut w
    | otherwise =
        simp (v : insert w{ interval = (xw1,xwc) } vs)
   where
    xv1 = fst (interval v)
    xw1 = fst (interval w)
    xv2 = snd (interval v)
    xw2 = snd (interval w)

    yv1 = snd (start v)
    yw1 = snd (start w)

    ~(Just xc) = line v `cross` line w
    
    -- what is the point beyond which w might be the minimum?
    xwc | slope (line v) > slope (line w) && xv1 < xc && xc < xv2 = xc
        | otherwise                                               = xv2
 
  simp [v] = [v]
  simp []  = []

cross :: Line -> Line -> Maybe Double
cross l m
  | slope l == slope m = Nothing
  | otherwise          = Just ((offset l - offset m) / (slope m - slope l))

----------------------------------------------------------------------
-- Combinators on segments and dists

-- Map a function over all values. The function must be linear.
mapValue :: (Double -> Double) -> Segment -> Segment
mapValue f (Point (x, d)) = Point (f x, d)
mapValue f s@Segment{interval = (x, y)} =
  segment (f x1, d1) (f x2, d2)
  where
    (x1, d1) = start s
    (x2, d2) = end s

-- Map a function over all distances. The function must be linear.
mapDistance :: (Double -> Double) -> Segment -> Segment
mapDistance f (Point (x, d)) = Point (x, f d)
mapDistance f s@Segment{line = l} =
  s{line = l{offset = f0, slope = f1 - f0}}
  where
    f0 = f (offset l)
    f1 = f (offset l + slope l)

-- Add a constant to all distances
plusDistance :: Double -> Segment -> Segment
plusDistance x = mapDistance (+ x)

-- Lift a unary function on segments to dists
lift1 :: (Segment -> [Segment]) -> Dist -> Dist
lift1 f a =
  Dist
  { val  = f0 (val a)
  , dist = norm $ sort [ q | p <- dist a, q <- f p ]
  }
 where
  f0 x = let (Point (y,_):_) = f (Point (x,0)) in y

-- Lift a binary function on segments to dists
lift2 :: (Segment -> Segment -> [Segment]) -> Dist -> Dist -> Dist
lift2 f a b =
  Dist
  { val  = f0 (val a) (val b)
  , dist = norm $ sort [ r | p <- dist a, q <- dist b, r <- f p q ]
  }
 where
  f0 x y = let (Point (z,_):_) = f (Point (x,0)) (Point (y,0)) in z

-- Lift a binary operation on Doubles to a binary operation on line segments.
-- One argument will be held to an endpoint of the line segment
-- while the other argument varies across the whole line segment.
-- The function should be linear when one argument is fixed.
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

-- Combine two segments by chopping them up into disjoint intervals
-- and equal intervals, and applying a function to each pair of intervals.
divideOverlaps ::
  (Segment -> Segment -> [Segment]) -> -- first segment comes before second
  (Segment -> Segment -> [Segment]) -> -- first segment comes after second
  (Segment -> Segment -> [Segment]) -> -- both segments have the same interval
  Segment -> Segment -> [Segment]
  -- Note: in the disjoint cases, the segments can share an endpoint.
  -- You can safely ignore this endpoint (i.e. treat the segments'
  -- intervals as being open at that end), since it will also be
  -- included in the "equal interval" case.
divideOverlaps before after common s1 s2
  | x > y =
    if fst (interval s1) < fst (interval s2) then before s1 s2 else after s1 s2
  | otherwise =
    concat $
      liftM2 before low1 (mid2:high2) ++
      liftM2 before [mid1] high2 ++
      [common mid1 mid2] ++
      liftM2 after high1 (mid2:low2) ++
      liftM2 after [mid1] low2
    where
      x = fst (interval s1) `max` fst (interval s2)
      y = snd (interval s1) `min` snd (interval s2)

      (low1, low2) = (low s1, low s2)
      (mid1, mid2) = (mid s1, mid s2)
      (high1, high2) = (high s1, high s2)

      low s@Segment{interval=(lo, hi)} =
        [s{interval=(lo, x)} | x > lo]
      mid s = s{interval = (x, y)}
      high s@Segment{interval=(lo, hi)} =
        [s{interval=(y, hi)} | y < hi]

-- Transform a segment by dividing it into positive, negative and zero parts,
-- and applying a function to each part.
divideSign ::
  (Segment -> [Segment]) -> -- positive part
  (Segment -> [Segment]) -> -- negative part
  (Segment -> [Segment]) -> -- zero part
  Segment -> [Segment]
divideSign pos neg zero s =
  divideOverlaps (\_ s -> pos s) (\_ s -> neg s) (\_ s -> zero s)
    (Point (0,0)) s

----------------------------------------------------------------------
-- Operations on dists

-- A constant value
constant :: Double -> Dist
constant x = Dist x [Point (x,0)]

-- An input in a particular range
input :: (Double,Double) -> Double -> Dist
input (a,b) x
 | a <= x && x <= b = Dist x $ segments [(a,x-a), (x,0), (b,b-x)]
 | otherwise        = error "input out of bounds"

-- Choose between two dists
ifThenElse :: Dist -> Dist -> Dist -> Dist
ifThenElse d1 d2 d3 =
  Dist {
    val = if val d1 == 1 then val d2 else val d3,
    dist = concatMap seg (dist d1) }
  where
    seg (Point (1, d)) =
      map (plusDistance d) (dist d2)
    seg (Point (0, d)) = 
      map (plusDistance d) (dist d3)

-- Numeric operations
instance Num Dist where
  fromInteger = constant . fromInteger
  (+) = plusDist
  (*) = timesDist
  negate = negateDist
  abs = absDist
  signum = signumDist

plusDist :: Dist -> Dist -> Dist
plusDist = lift2 (combineEndpoints (+))

timesDist :: Dist -> Dist -> Dist
timesDist = lift2 timesS
  where
    timesS s1 s2 =
      squareRootSegment s1 s2 ++
      combineEndpoints (*) s1 s2

    squareRootSegment s1@Segment{line = Line{slope = a}} s2@Segment{line = Line{slope = b}}
      | not (isPoint s1) && not (isPoint s2) && a /= 0 && b /= 0 =
        map (mapValue (/(a*b))) (squareRootSegment' (mapValue (*a) s1) (mapValue (*b) s2))
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

negateDist :: Dist -> Dist
negateDist = lift1 (return . mapValue negate)

absDist :: Dist -> Dist
absDist = lift1 (divideSign pos neg zero)
  where
    pos s = [s]
    neg s = [mapValue negate s]
    zero s = [s]

signumDist :: Dist -> Dist
signumDist = lift1 (divideSign pos neg zero)
  where
    pos s = [mapValue (const 1) s]
    neg s = [mapValue (const (-1)) s]
    zero s = [mapValue (const 0) s]

-- Comparisons

(<=?) :: Dist -> Dist -> Dist
(<=?) = lift2 (divideOverlaps before after common)
  where
    before = constCase 1
    after = constCase 0
    common s1 s2 =
      concat
        [ pointCase s1 s2 x1 x2
        | x1 <- endpoints s1
        , x2 <- endpoints s2 ]

    constCase val s1 s2 = [Point (val, minDistance s1 + minDistance s2)]

    pointCase s1 s2 x1 x2 =
      [Point (1, d) | x1 <= x2] ++
      [Point (0, d) | x1 >  x2] ++
      -- Case where the interval is not a point, and the x-values are
      -- the same. In this case, we can move x1 or x2 a little bit to
      -- make x1 > x2.
      [Point (0, d) | x1 == x2 && not (isPoint s1)]
      where
        d1 = line s1 `at` x1
        d2 = line s2 `at` x2
        d = d1 + d2

    endpoints Segment{interval = (x, y)} = [x, y]

(==?) :: Dist -> Dist -> Dist
(==?) = lift2 eqS
  where
    eqS (Point (x1, d1)) (Point (x2, d2))
      | x1 == x2  = [Point (1, d1+d2)]
      | otherwise = [Point (0, d1+d2)]
    eqS s1 s2 = 
      -- non-point intervals can always be made unequal
      [ Point (0, minDistance s1 + minDistance s2) ] ++
      -- overlapping intervals - try both ends of the overlap
      [ Point (1, line s1 `at` z + line s2 `at` z)
      | z1 <= z2, z <- [z1, z2] ]
      where
        z1 = fst (interval s1) `max` fst (interval s2)
        z2 = snd (interval s1) `min` snd (interval s2)

-- Minimum - naive implementation
minn' :: Dist -> Dist -> Dist
minn' d1 d2 =
  ifThenElse (d1 <=? d2) d1 d2

-- Minimum - better implementation
minn :: Dist -> Dist -> Dist
minn = lift2 (divideOverlaps before after common)
  where
    before s1 s2 = [plusDistance (minDistance s2) s1]
    after s1 s2 = [plusDistance (minDistance s1) s2]
    common s1 s2 =
      -- Try keeping one input as high as possible, or moving them in lockstep
      [plusDistance (snd (end s1)) s2,
       plusDistance (snd (end s2)) s1,
       Segment{interval = interval s1,
               line = Line { slope = slope (line s1) + slope (line s2),
                             offset = offset (line s1) + offset (line s2) }}]

-- Find the distance of a dist to a given value
eval :: Dist -> Double -> Double
eval d x = search (dist d)
  where
    search [] = 1/0
    search (Segment{interval = (lo,hi), line = l}:ss)
      | x < lo  = 1/0
      | x < hi  = l `at` x
      | x == hi = (l `at` x) `min` search ss -- overlapping case
      | otherwise = search ss

----------------------------------------------------------------------
-- Visualising dists

-- Plot a dist on screen
plotDist :: Dist -> IO Bool
plotDist d =
  plot X11 $ map plotSeg (dist d)
  where
    plotSeg (Point (x, d)) =
      Data2D [] [] [(x, d)]
    plotSeg Segment{line = l, interval = (x, y)} =
      Function2D [] [Range x y, Step ((y-x)/10000)] (\x -> slope l*x + offset l)

-- Show the value and robustness of a Boolean dist
showBool :: Dist -> String
showBool d =
  show (val d == 1) ++ " with robustness " ++ show (eval d (1 - val d))
