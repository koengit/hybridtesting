-- Describing the set of test inputs for a system.
{-# LANGUAGE DeriveGeneric, TupleSections #-}
module Process.Input where

import Process.Language(Var)
import Process.Eval
import qualified Data.Map.Strict as Map
import Data.Map (Map)
import Data.List( transpose )
import GHC.Generics
import Test.QuickCheck
import Data

--------------------------------------------------------------------------------
-- types

data Shape
  = Continuous  -- can change at every time step               ~~^-'\/
  | Discrete    -- only changes at a finite number of steps    --__-_¨
  | Parameter   -- keeps its value during the entire execution _______
 deriving (Eq, Ord, Show, Generic)

instance Data Shape

data Type
  = Real    (Double,  Double)
  | Integer (Integer, Integer)
  | Bool
 deriving (Eq, Ord, Show)

type Types = Map Var (Shape, Type)

--------------------------------------------------------------------------------
-- signal

type Duration = Double -- >= 0

data Signal
  = Point Value Duration Signal
  | End Value
 deriving (Eq, Ord, Generic)

instance Data Signal where
  vals (Point v d s) = vals v ++ vals d ++ vals s
  vals (End v)       = vals v
  
  fill (Point v d s) xs = Point (fill v xs) (0 `max` fill d (drop k xs))
                                (fill s (drop (k+1) xs)) where k = length (vals v)
  fill (End v)       xs = End (fill v xs)

instance Show Signal where
  show (Point v dur sig) = show v ++ " (" ++ show dur ++ "s) " ++ show sig
  show (End v)           = show v

values :: Signal -> [Value]
values (Point v _ s) = v : values s
values (End v)       = [v]

durations :: Signal -> [Double]
durations (Point _ d s) = d : durations s
durations (End _)       = []

sampleSignal :: Double -> Shape -> Signal -> [Value]
sampleSignal delta Parameter sig = repeat (head (values sig))
sampleSignal delta shape     sig = segments 0 sig
 where
  segments _   (End v)           = repeat v
  segments off (Point v dur sig) = vs ++ segments off' sig
   where
    (vs,off') = line off v dur (head (values sig))

  line off v0 dur v1 =
    (map snd tvs, if null tvs
                    then off - dur
                    else dur - fst (last tvs))
   where
    tvs = [ (t,v)
          | t <- takeWhile (<= dur) [off,off+delta..]
          , let v = case shape of
                      Discrete   -> v
                      Continuous -> DoubleValue (doubleValue v0 + t * a)
                
                a | dur > 0   = (doubleValue v1 - doubleValue v0) / dur
                  | otherwise = 1 -- t will be 0
          ]

--------------------------------------------------------------------------------
-- input

data Input
  = Input Duration (Map Var (Shape, Signal))
 deriving (Generic)

instance Show Input where
  show (Input dur signals) =
    unlines $
    [ "Input of " ++ show dur ++ "s:" ] ++
    [ "  " ++ show v ++ ": " ++ show s
    | (v,(_,s)) <- Map.toList signals
    ]

instance Data Input where
  vals (Input dur mp)    = vals mp
  fill (Input dur mp) xs = Input dur (fill mp xs)

mkInput :: Duration -> (Map Var (Shape, Signal)) -> Input
mkInput dur sigs = Input (max 0 dur) (Map.map (\(sh,sig) -> (sh, cut 0 sig)) sigs)
 where
  cut t sig | t >= dur  = End (head (values sig))
  cut t (End v)         = End v
  cut t (Point v d sig) = Point v d (cut (t+d) sig)

sampleInput :: Valued f => Double -> Input -> [Env f]
sampleInput delta (Input dur sigs) =
    take (truncate (dur / delta)) 
  . map Map.fromList
  . transpose
  . map (\(v,(sh,sig)) -> map ((v,) . val) (sampleSignal delta sh sig))
  . Map.toList
  $ sigs

--------------------------------------------------------------------------------
-- QuickCheck functions

-- arbitrary

genValue :: Type -> Gen Value
genValue (Real (x, y)) =
  DoubleValue <$> clamp <$> choose (x - delta, y + delta)
  where
    delta = (y - x) / 10
    clamp z
      | z < x = x
      | z > y = y
      | otherwise = z
genValue (Integer (x, y)) = DoubleValue <$> fromInteger <$> choose (x, y)
genValue Bool = BoolValue <$> arbitrary

-- generates an infinite signal always
genSignal :: Gen Duration -> Gen Value -> Gen Signal
genSignal genDur genVal =
  do v <- genVal
     d <- genDur
     s <- genSignal genDur genVal
     return (Point v d s)

genInput :: Duration -> Types -> Gen Input
genInput maxdur types =
  do --dur <- sized $ \n -> choose (0, maxdur*fromIntegral n/100)
     let dur = maxdur
     let genDur = (^2) <$> choose (0, sqrt dur)
     mkInput dur <$> mapM (\(sh,ty) -> (sh,) <$> genSignal genDur (genValue ty)) types

-- shrinking

-- (I decided to not shrink values, only the complexity of the functions and the durations)

shrinkDur :: Duration -> [Duration]
shrinkDur d =
  [ d'
  | d' <- [d/2,fromIntegral (truncate d),d-1]
  , 0 <= d'
  , d' < d
  ]

shrinkSignal :: Signal -> [Signal]
shrinkSignal (End v)       = []
shrinkSignal (Point v d s) =
  [ s ] ++
  [ End v'
  | End w <- [s]
  , v' <- combine v w
  ] ++
  [ Point v' (d+d') s'
  | Point w d' s' <- [s]
  , v' <- combine v w
  ] ++
  [ Point v d' s | d' <- shrinkDur d ] ++
  [ Point v d s' | s' <- shrinkSignal s ]
 where
  combine v w = [v,w]
             ++ [ DoubleValue ((a+b)/2)
                | DoubleValue a <- [v]
                , DoubleValue b <- [w]
                ]

shrinkInput :: Input -> [Input]
shrinkInput (Input dur sigs) =
  [ Input dur (Map.insert x (sh,sig') sigs)
  | (x,(sh,sig)) <- Map.toList sigs
  , sig' <- shrinkSignal sig
  ] ++
  [ mkInput dur' sigs
  | dur' <- shrinkDur dur
  ]

--------------------------------------------------------------------------------

