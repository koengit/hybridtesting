{-# LANGUAGE DeriveGeneric #-}
module Val where

import qualified Data.Map as M
import Data.List( sort, sortBy, intercalate )
import Data.Ord
import System.Process( system )

--------------------------------------------------------------------------------
-- Val keeps track of a value, plus alternatives

data Val a = Val{ the :: a, alts :: [(a,Double{- >=0 -})] }
 deriving ( Eq, Ord, Show )

val :: a -> Val a
val x = Val x []

mkVal :: Ord a => a -> [(a,Double)] -> Val a
mkVal x xs = Val x (M.toList (M.fromListWith op (filter ((x/=).fst) xs)))
 where
  op = min
  --op = par
 
  x `par` y = recip (recip x + recip y)

--------------------------------------------------------------------------------
-- basic lifting

mapVal :: Ord b => (a->b) -> Val a -> Val b
mapVal f (Val x xs) =
  mkVal (f x)
  [ (f x, a)
  | (x,a) <- xs
  ]

liftVal :: Ord c => (a->b->c) -> Val a -> Val b -> Val c
liftVal f (Val x xs) (Val y ys) =
  mkVal (f x y) $
  [ (f x y, a + b)
  | (x,a) <- xs
  , (y,b) <- ys
  ] ++
  [ (f x y, a) | (x,a) <- xs ] ++
  [ (f x y, b) | (y,b) <- ys ]

smashVal :: Ord a => Val (Val a) -> Val a -- monadic join
smashVal (Val (Val v vs) ws) =
  mkVal v $
  [ (z, a + b)
  | (Val u us,a) <- ws
  , (z,b) <- (u,0) : us
  ] ++
  vs

--------------------------------------------------------------------------------
-- basic operations

(||?), (&&?), (=>?) :: Val Bool -> Val Bool -> Val Bool
(||?) = liftVal (||)
(=>?) = liftVal (<=) -- this is correct: F<=F, F<=T, T<=T but not(T<=F)
(&&?) = liftVal (&&)
nott  = mapVal not

instance (Ord a, Num a) => Num (Val a) where
  (+)         = liftVal (+)
  (-)         = liftVal (-)
  (*)         = liftVal (*)
  abs         = mapVal abs
  negate      = mapVal negate
  signum      = mapVal signum
  fromInteger = val . fromInteger

instance (Ord a, Fractional a) => Fractional (Val a) where
  (/)          = liftVal (/)
  recip        = mapVal recip
  fromRational = val . fromRational

instance (Ord a, Floating a) => Floating (Val a) where
  pi    = val pi
  exp   = mapVal exp
  log   = mapVal log
  sin   = mapVal sin
  cos   = mapVal cos
  asin  = mapVal asin
  acos  = mapVal acos
  atan  = mapVal atan
  sinh  = mapVal sinh
  cosh  = mapVal cosh
  asinh = mapVal asinh
  acosh = mapVal acosh
  atanh = mapVal atanh

class VCompare a where
  (==?), (/=?), (>?), (>=?), (<?), (<=?) :: a -> a -> Val Bool

  x /=? y = mapVal not (x ==? y)
  x >=? y = y <=? x
  x >?  y = y <? x
  x <?  y = mapVal not (y <=? x)

instance VCompare Double where
  x ==? y = eqZero (y - x)
  x <=? y = geqZero (y - x)

eqZero :: Double -> Val Bool
eqZero x
  | x == 0    = Val True  [(False,0)]
  | otherwise = Val False [(True,abs x)]

geqZero :: Double -> Val Bool
geqZero x
  | x >= 0    = Val True  [(False,x)]
  | otherwise = Val False [(True,-x)]

instance VCompare a => VCompare (Val a) where
  (==?) = compVal (==?)
  (/=?) = compVal (/=?)
  (>?)  = compVal (>?)
  (>=?) = compVal (>=?)
  (<?)  = compVal (<?)
  (<=?) = compVal (<=?)

compVal op x y =
  smashVal (liftVal op x y)

--------------------------------------------------------------------------------

class Choice a where
  ifThenElse :: Val Bool -> a -> a -> a

instance Choice Double where
  ifThenElse c x y =
    if the c then x else y -- loses distance info

instance Ord a => Choice (Val a) where
  ifThenElse (Val c cs) (Val x xs) (Val y ys) =
    mkVal (if c then x else y) $
    [ (z,dc + dz)
    | (c,dc) <- cs
    , (z,dz) <- if c then (x,0):xs else (y,0):ys
    ] ++
    if c then xs else ys

--------------------------------------------------------------------------------

forget :: Ord a => Val a -> Val a
forget (Val x xs) =
--  (if n >= 2 then trace ("(alts:" ++ show n ++ ")") else id) $
    Val x (take 100 (sortBy (comparing best) xs))
 where
  n = length xs
  best (_,v) = v

howTrue :: Val Bool -> Double
howTrue (Val True  [])          = infinity
howTrue (Val True  [(False,d)]) = d + 1
howTrue (Val False [])          = -infinity
howTrue (Val False [(True,d)])  = -d - 1

infinity :: Double
infinity = 1/0

--------------------------------------------------------------------------------

{-
f :: (Num a, VCompare a, Choice a) => a -> a
f x =
  ifThenElse (x <? a) 10
    (ifThenElse (x >? (a+d)) 20
      (-10))

f0 :: (Fractional a, Num a, VCompare a, Choice a) => a -> a
f0 x = 0.01*x^2 -x +25 - 0.000013*x^3

f1 :: (Fractional a, Num a, VCompare a, Choice a) => a -> a
f1 x =
  ifThenElse ((x <? 200) ||? (x >? 300)) (0.01*x^2 -x +25 - 0.000013*x^3) $
  (0.01*x^2 -x -20 - 0.000013*x^3)

f2 :: (Fractional a, Floating a, Num a, VCompare a, Choice a) => a -> a
f2 x = sin ((x-50) / 45) * (x / 200) + 1.2

f3 :: (Fractional a, Floating a, Num a, VCompare a, Choice a) => a -> a
f3 x =
  (ifThenElse ((x <? 200) ||? (x >? 300)) 1 $
    sin (x * 0.02 *pi + 0.5*pi)
  ) * 100 + 99

f4 :: (Fractional a, Floating a, Num a, VCompare a, Choice a) => a -> a
f4 x =
  (ifThenElse (x <? 200) 1 $
   ifThenElse (x >? 300) 1 $
    sin (x * 0.02 *pi + 0.5*pi)
  ) * 100 + 99

g :: (Fractional a, VCompare a, Choice a) => a -> a
g x =
  ifThenElse (x <? a) (10 `max` x)
    (ifThenElse (x >? (a+d)) (10 `max` (2*a+d - x))
      (-10))
 where
  x `max` y =
    ifThenElse (x >=? y) x y

h :: (Num a, VCompare a, Choice a) => a -> a
h x =
  ifThenElse (x <=? a) 10 $
  ifThenElse (x <=? (a+d)) (-10) $
  ifThenElse (x <=? (2*a)) 20 $
  ifThenElse (x <=? (2*a+d)) (-10) $
    15

a, d :: Num a => a
a = 202
d = 1

prop_Basic f x =
  withBadness $
  let (y,b) = forData x (\x -> propVal (f (x :: Double) >=? 0)) in
    whenFail (print y) (VBool.isTrue b)

prop_Val f x =
  withBadness $
  let (y,b) = forData x (\x -> propVal (f (val x) >=? 0)) in
    whenFail (print y) (VBool.isTrue b)

plot :: (Double,Double) -> [(String, Double -> Double)] -> IO ()
plot (xL,xR) fs =
  do sequence_
       [ writeFile ("plot-" ++ name ++ ".xy") $ unlines $
           [ show x ++ " " ++ show y
           | (x,y) <- xs `zip` ys
           ]
       | ((name,_),ys) <- fs `zip` yss
       ]
     writeFile "plot.in" $ unlines $
       [ "set terminal pdf enhanced font 'Times,18' lw 3"
       , "set grid"
       --, "set yrange [0:]"
       , "set autoscale x"
       , "set yrange [" ++ show miny ++ ":" ++ show maxy ++ "]"
       , "set output 'plot.pdf'"
       ] ++
       [ "plot " ++
         intercalate ", "
         [ "'plot-" ++ name ++ ".xy' with lines title '" ++ name ++ "'"
         | (name,_) <- fs
         ]
       ]
     system "gnuplot < plot.in"
     return ()
 where
  dx  = (xR-xL) / 1000
  xs  = [xL,xL+dx..xR]
  yss = [ map f xs | (_,f) <- fs ]

  minY = minimum (concat yss)
  maxY = maximum (concat yss)
  dy   = (maxY-minY) / 33 -- 3%
  miny = minY-dy
  maxy = maxY+dy

plotf f fR = plot (-100,500)
             [ ("f", f)
             , ("fT", \x -> VBool.howTrue (propVal (fR (val x) >=? 0)))
             ]

plot3D :: (Double,Double) -> (Double,Double)
       -> [(String, (Double,Double) -> Double)] -> IO ()
plot3D (xL,xR) (yL,yR) fs =
  do sequence_
       [ writeFile ("plot-" ++ name ++ ".xyz") $ unlines $
           [ show x ++ " " ++ show y ++ " " ++ show z
           | ((x,y),z) <- xys `zip` zs
           ]
       | ((name,_),zs) <- fs `zip` zss
       ]
     writeFile "plot.in" $ unlines $
       [ "set terminal pdf enhanced font 'Times,18' lw 3"
       , "set grid"
       , "set autoscale xy"
       , "set zrange [" ++ show minz ++ ":" ++ show maxz ++ "]"
       , "set output 'plot3d.pdf'"
       , "set dgrid3d 30,30"
       , "set hidden3d"
       ] ++
       [ "splot " ++
         intercalate ", "
         [ "'plot-" ++ name ++ ".xyz' with lines title '" ++ name ++ "'"
         | (name,_) <- fs
         ]
       ]
     system "gnuplot < plot.in"
     return ()
 where
  dx  = (xR-xL) / 30
  dy  = (yR-yL) / 30
  xs  = [xL,xL+dx..xR]
  ys  = [yL,yL+dy..yR]
  xys = [ (x,y) | x <- xs, y <- ys ]
  zss = [ map f xys | (_,f) <- fs ]

  minZ = minimum (concat zss)
  maxZ = maximum (concat zss)
  dz   = (maxZ-minZ) / 33 -- 3%
  minz = minZ-dz
  maxz = maxZ+dz
-}

----

{-
data State = A | B | C deriving ( Show, Eq, Ord )

ship :: [Val Double] -> [Val State]
ship as = ss
 where
  vs = zipWith (+) (pre 0 vs) as
  xs = zipWith (+) (pre 0 xs) vs
  ss = [ ifThenElse (x <=? (-100)) (val A) $
           ifThenElse (x >=? 100) (val C)
             (val B)
       | x <- xs
       ]

observer :: [Val State] -> [Val Bool]
observer ss = ok
 where
  seenA = zipWith (||?)
            (pre (vbool false) seenA)
            (map (liftVal (==) (val A)) ss)
  seenC = zipWith (||?)
            (pre (vbool false) seenC)
            (map (liftVal (==) (val C)) ss)
  ok    = [ liftVal (/=) s s' &&?
            sA &&?
            sC
          | ((s,s'),(sA,sC)) <- (ss `zip` pre (val B) ss) `zip`
                                (pre (vbool false) seenA
                                  `zip` pre (vbool false) seenC)
          ]

ship2 :: [Val Double] -> [Val State]
ship2 as = ss
 where
  vs = zipWith (+) (pre 0 vs) as
  xs = zipWith (+) (pre 0 xs) vs
  ss = map (forget (const ())) $ pre (val B) (zipWith h xs ss)

  h x s =
    ifThenElse (x <=? (-100)) (val A) $
    ifThenElse (x >=? 100)    (val C) $
      s

pre x xs = x:xs

data Ps
  = Piece Int Double Ps
  | End
 deriving ( Eq, Show, Ord, Generic )

instance Data Ps

instance Arbitrary Ps where
  arbitrary =
    do k <- return 4 -- arbitrary
       dxs <- sequence [ do d <- choose (0,50)
                            x <- choose (-1,1)
                            return (d,x)
                       | i <- [0..k::Int]
                       ]
       return (foldr (uncurry Piece) End dxs)

  shrink (Piece k x q) =
    []
    {-
    [ q ] ++
    [ Piece (k+k') z q
    | k > 0
    , Piece k' y _ <- [q]
    , k' > 0
    , z <- [ (fromIntegral k * x + fromIntegral k' * y)/fromIntegral (k+k')
           ]
    ] ++
    [ Piece k x q'
    | q' <- shrink q
    ]
    -}

pieces :: Ps -> [Double]
pieces End             = []
pieces (Piece _ x End) = repeat x
pieces (Piece k x q)   = replicate k x ++ pieces q

values :: Ps -> [Double]
values End           = []
values (Piece _ x q) = x : values q

f_Ship pv bs =
  foldr (&&+) true [ (-1) <=% x &&+ x <=% 1 | x <- values bs ] ==>%
    (pv $ nott $ foldr (||?) (vbool false) ps)
 where
  ps = observer (ship (map val (take 80 (pieces bs))))

prop_Ship bs =
  withBadness $
    let (y,b) = forData bs (f_Ship propVal)
     in whenFail (do print y; print b) $
          isTrue $
            b

main1 = quickCheck prop_Ship

main2 = plot (-1,1)
        [ ("p",  \a -> howTrue $ f_Ship propVal0 $ accs a)
        -- , ("pT", \a -> howTrue $ f_Ship propVal  $ accs a)
        ]

accs a = Piece 15 (a-1)
       $ Piece 50 a
       $ Piece 60 (-1)
       $ End

prop_Ship' a =
  withBadness $
    let (y,b) = forData a (\a -> f_Ship propVal (accs a))
     in whenFail (print y) $
          isTrue $
            b

main3 = quickCheck prop_Ship'

--main = main2
main = main1
--main = do main2 --; main3
-}

