module Dist where

import Data.List( insert, sort, sortBy, group )
import qualified Data.Set as S
import Data.Ord( comparing )
import Control.Monad( guard )

data Dist
  = Dist
  { val  :: Double
  , dist :: [Piece] -- strictly ordered
  }
 deriving ( Eq, Ord, Show )

data Piece
  = Segment{ line :: Line, interval :: Interval } -- open, >0
  | Point{ point :: Point }
 deriving ( Eq, Show )

data Line
  = Line{ slope :: Double, offset :: Double }
 deriving ( Eq, Show )

type Interval = (Double, Double)
type Point = (Double, Double)

start, end :: Piece -> Point
start Segment{interval = (x, _), line = l} = (x, l `at` x)
start (Point p) = p

end Segment{interval = (_, x), line = l} = (x, l `at` x)
end (Point p) = p

-- Assumes x1 < x2
lineThrough :: Point -> Point -> Line
lineThrough (x1, y1) (x2, y2) =
  let slope = (y2-y1)/(x2-x1)
  in Line{ slope = slope, offset = y1 - slope*x1}

openSegment :: Point -> Point -> [Piece]
openSegment p q =
  case comparing fst p q of
    LT -> [Segment{ line = lineThrough p q, interval = (fst p, fst q) }]
    EQ -> []
    GT -> [Segment{ line = lineThrough q p, interval = (fst q, fst p) }]

segments :: [Point] -> [Piece]
segments [p] = [Point p]
segments (p:q:ps) | fst p == fst q =
  segments $ (if snd p < snd q then p else q):ps
segments (p:q:ps) =
  [Point p] ++ openSegment p q ++ segments (q:ps)

combineEndpoints :: (Point -> Point -> Point) -> Piece -> Piece -> [Piece]
combineEndpoints f (Point p) (Point q) =
  [Point (f p q)]
combineEndpoints f (Point p) s@Segment{} =
  openSegment (f p q1) (f p q2)
  where
    q1 = start s
    q2 = end s
combineEndpoints f s@Segment{} (Point q) =
  openSegment (f p1 q) (f p2 q)
  where
    p1 = start s
    p2 = end s
combineEndpoints f s@Segment{} t@Segment{} =
  concat [ openSegment (uncurry f x) (uncurry f y) | (x, y) <- pairs ]
  where
    pairs = nub
      [((p, q1), (p, q2)) | p <- [p1, p2]] ++
      [((p1, q), (p2, q)) | q <- [q1, q2]]
    p1 = start s
    p2 = end s
    q1 = start t
    q2 = end t

distAdd :: Dist -> Dist -> Dist
distAdd = lift2 (combineEndpoints (\(x1, d1) (x2, d2) -> (x1+x2, d1+d2)))

distMul :: Dist -> Dist -> Dist
distMul = lift2 pieceMul
  where
    pieceMul p q =
      squareRootPiece p q ++
      combineEndpoints (\(x1, d1) (x2, d2) -> (x1 * x2, d1 + d2)) p q

    squareRootPiece s1@Segment{} s2@Segment{} = do
      guard (a /= 0 && b /= 0)
      s1'@Segment{} <- scale a s1
      s2'@Segment{} <- scale b s2
      s <- squareRootPiece' s1' s2'
      scale (1/(a*b)) s
     where
      a = slope (line s1)
      b = slope (line s2)
    squareRootPiece _ _ = []

    squareRootPiece' s1 s2
      | 0 <= z1 && z1 < z2 =
        map (mapDistance (\d -> 2*(d-z1) + line s1 `at` z1 + line s2 `at` z1)) (squareRootApprox z1 z2)
      | otherwise = []
      where
        z1 = fst (interval s1) `max` fst (interval s2)
        z2 = snd (interval s1) `min` snd (interval s2)

    -- approximate sqrt function between a^2 and c^2
    squareRootApprox :: Double -> Double -> [Piece]
    squareRootApprox a c =
      segments [(a^2, a), (b^2, b), (c^2, c)]
      where
        b = (a+c)/2 -- minimises absolute error

scale :: Double -> Piece -> [Piece]
scale x = mapValue (* x)

-- f must be linear
mapValue :: (Double -> Double) -> Piece -> [Piece]
mapValue f (Point (x, d)) = [Point (f x, d)]
mapValue f s@Segment{interval = (x, y)} =
  openSegment (f x1, d1) (f x2, d2)
  where
    (x1, d1) = start s
    (x2, d2) = end s

-- f must be linear
mapDistance :: (Double -> Double) -> Piece -> Piece
mapDistance f (Point (x, d)) = Point (x, f d)
mapDistance f s@Segment{line = l} =
  s{line = l{offset = f0, slope = f1 - f0}}
  where
    f0 = f (offset l)
    f1 = f (offset l + slope l)

instance Ord Piece where
  compare = comparing tuple
   where
    tuple p = (start p, mslope p, -snd (interval p))

constant :: Double -> Dist
constant x = Dist x [Point (x,0)]

input :: (Double,Double) -> Double -> Dist
input (a,b) x
 | a <= x && x <= b = Dist x $ segments [(a,x-a), (x,0), (b,b-x)]
 | otherwise        = error "input out of bounds"

lift2 :: (Piece -> Piece -> [Piece]) -> Dist -> Dist -> Dist
lift2 f a b =
  Dist
  { val  = f0 (val a) (val b)
  , dist = {-norm-} [ r | p <- dist a, q <- dist b, r <- f p q ]
  }
 where
  f0 x y = let [Point (z,_)] = f (Point (x,0)) (Point (y,0)) in z
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

mslope :: Piece -> Maybe Double
mslope (Point _) = Nothing
mslope s@Segment{} = Just (slope (line s))

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

