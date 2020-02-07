import Process
import qualified Data.Map as Map
import Process.QuickCheck
import Test.QuickCheck

thrust :: Var
thrust = Global "thrust"

position :: Var
position = Global "position"

mass :: Var
mass = Global "mass"

ship :: Process
ship =
  continuous position 0 (integral (integral (var thrust/var mass)))

check :: (Double, Double, Double) -> Process
check (g1, g2, g3) =
  sequential (first (set mass (double g1))) (var position >=? 100) $
  sequential (first (set mass (double g2))) (var position <=? -100) $
  --process skip $ assert "reached destination" (var position <=? 100)
  sequential (first (set mass (double g3))) (var position >=? 100) $
    first (assert "reached destination" false)

test :: (Show (f Bool), Valued f, Ord (f Value)) => (Double, Double, Double) -> [Double] -> [Env f]
test g vals = simulate 1 envs (lower stdPrims $ ship & check g)
  where
    envs = [Map.singleton thrust (val (DoubleValue x)) | x <- vals]

prop_SpaceShip g =
--checkAssertions :: Double -> Duration -> Types -> Process -> Property
  --checkAssertions 1 1000 (Map.singleton thrust (Continuous, Real (-10,10))) $
  checkAssertionsVal 1 100 (Map.singleton thrust (Continuous, Real (-5,5))) $
    lower stdPrims $ simplify $ ship & check g

main = quickCheckWith stdArgs{ maxSuccess = 100000 } (prop_SpaceShip (1, 2, 1))
main' =
  checkAssertionsIO 1 100 (Map.singleton thrust (Continuous, Real (-5,5))) $
    lower stdPrims $ simplify $ ship & check (1,0.5,2)

