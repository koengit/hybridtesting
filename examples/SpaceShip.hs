import Process
import qualified Data.Map as Map
import Process.QuickCheck
import Test.QuickCheck

acceleration :: Var
acceleration = Global "acceleration"

position :: Var
position = Global "position"

ship :: Process
ship = continuous position 0 (integral (integral (var acceleration)))

check :: Process
check =
  sequential skip (var position >=? 100) $
  sequential skip (var position <=? -100) $
-- process skip $ assert "reached destination" (var position <=? 100)
  sequential skip (var position >=? 100) $
    first (assert "reached destination" false)

test :: (Show (f Bool), Valued f) => [Double] -> [Env f]
test vals = simulate 1 envs (lower stdPrims $ ship & check)
  where
    envs = [Map.singleton acceleration (val (DoubleValue x)) | x <- vals]

prop_SpaceShip =
--checkAssertions :: Double -> Duration -> Types -> Process -> Property
--  checkAssertions 0.1 100 (Map.singleton acceleration (Continuous, Real (-5,5))) $
  checkAssertionsVal 0.1 100 (Map.singleton acceleration (Continuous, Real (-5,5))) $
    lower stdPrims $ ship & check

main = quickCheck prop_SpaceShip

