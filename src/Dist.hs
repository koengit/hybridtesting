module Dist where

import Data.List( insert, sort, sortBy, group )
import qualified Data.Set as S
import Data.Ord( comparing )

data Dist
  = Dist
  { val  :: Double
  , dist :: [Piece] -- strictly ordered
  }
 deriving ( Eq, Ord, Show )

data Piece
  = Line{ start :: (Double,Double), end_ :: (Double,Double) } -- open, >0
  | Point{ start :: (Double,Double) }
 deriving ( Eq, Show )

end :: Piece -> (Double,Double)
end (Line _ x) = x
end (Point x) = x

value, distance :: (Double, Double) -> Double
value = fst
distance = snd

openSegment :: (Double, Double) -> (Double, Double) -> [Piece]
openSegment (x1,d1) (x2,d2) =
  case compare x1 x2 of
    LT -> [Line (x1,d1) (x2,d2)]
    EQ -> []
    GT -> [Line (x2,d2) (x1,d1)]

segments [p] = [Point p]
segments (p:q:ps) | value p == value q =
  segments $ (if distance p < distance q then p else q):ps
segments (p:q:ps) = Point p:Line p q:segments (q:ps)

combineEndpoints :: ((Double, Double) -> (Double, Double) -> (Double, Double)) -> Piece -> Piece -> [Piece]
combineEndpoints f (Point p) (Point q) =
  [Point (f p q)]
combineEndpoints f (Point p) (Line q1 q2) =
  openSegment (f p q1) (f p q2)
combineEndpoints f (Line p1 p2) (Point q) =
  openSegment (f p1 q) (f p2 q)
combineEndpoints f (Line p1 p2) (Line q1 q2) =
  concat [ openSegment (uncurry f x) (uncurry f y) | (x, y) <- pairs ]
  where
    pairs = nub
      [((p, q1), (p, q2)) | p <- [p1, p2]] ++
      [((p1, q), (p2, q)) | q <- [q1, q2]]

distAdd :: Dist -> Dist -> Dist
distAdd = lift2 (combineEndpoints (\(x1, d1) (x2, d2) -> (x1+x2, d1+d2)))

distMul :: Dist -> Dist -> Dist
distMul = lift2 pieceMul
  where
    pieceMul p q =
      squareRootPiece p q ++
      combineEndpoints (\(x1, d1) (x2, d2) -> (x1 * x2, d1 + d2)) p q

    squareRootPiece l1 l2 =
      case (slope l1, slope l2) of
        (Just a, Just b) | not (a == 0 || b == 0) ->
          map (scale (a*b)) (squareRootPiece' (scale (1/a) l1) (scale (1/b) l2))
        _ -> []
      where
        a = slope l1
        b = slope l2

    squareRootPiece' l1 l2
      | 0 <= z1 && z1 < z2 =
        concatMap (mapDistance (\d -> 2*(d-z1) + l1 `at` z1 + l2 `at` z1)) (squareRootApprox z1 z2)
      | otherwise = []
      where
        z1 = value (start l1) `max` value (start l2)
        z2 = value (end l1)   `min` value (end l2)

    -- approximate sqrt function between a^2 and c^2
    squareRootApprox :: Double -> Double -> [Piece]
    squareRootApprox a c =
      segments [(a^2, a), (b^2, b), (c^2, c)]
      where
        b = (a+c)/2 -- minimises absolute error

scale :: Double -> Piece -> Piece
scale x = mapValue (* x)

mapValue :: (Double -> Double) -> Piece -> Piece
mapValue f (Point (x, d)) = Point (f x, d)
mapValue f (Line (x1, d1) (x2, d2)) =
  case openSegment (f x1, d1) (f x2, d2) of
    [p] -> p

mapDistance :: (Double -> Double) -> Piece -> [Piece]
mapDistance f (Point (x, d)) = [Point (x, f d)]
mapDistance f (Line (x1, d1) (x2, d2)) = openSegment (x1, f d1) (x2, f d2)

instance Ord Piece where
  compare = comparing tuple
   where
    tuple p = (start p, slope p, -value (end p))

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
  , dist = norm [ r | p <- dist a, q <- dist b, r <- f p q ]
  }
 where
  f0 x y = let [Point (z,_)] = f (Point (x,0)) (Point (y,0)) in z

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

at :: Piece -> Double -> Double
Point (_, a) `at` _ = a
Line (x1,a1) (x2,a2) `at` x =
  a1 + (x - x1) * (a2 - a1) / (x2 - x1)

slope :: Piece -> Maybe Double
slope (Point _) = Nothing
slope (Line (x1,a1) (x2,a2)) = Just ((a2-a1)/(x2-x1))

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

