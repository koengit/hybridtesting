import Process
import qualified Data.Map as Map
import Process.QuickCheck
import Test.QuickCheck

acceleration :: Var
acceleration = Global "acceleration"

position :: Var
position = Global "position"

state :: Var
state = Global "state"

ship :: (Double, Double, Double) -> Process
ship g =
  continuous position 0 (integral (integral (gravity g*var acceleration)))

gravity :: (Double, Double, Double) -> Expr
gravity (g1, g2, g3) =
  cond (var state ==? 1) (double g1) $
  cond (var state ==? 2) (double g2) (double g3)

check :: Process
check =
  (continuous state 1 $
    cond (var state ==? 1 &&& var position >=? 100) 2 $
    cond (var state ==? 2 &&& var position <=? -100) 3 $
    var state) &
  loop (assert "reached destination" (var state /=? 3 ||| var position <=? 100))

test :: (Show (f Bool), Valued f, Ord (f Value)) => (Double, Double, Double) -> [Double] -> [Env f]
test g vals = simulate 0.1 envs (lower stdPrims $ ship g & check)
  where
    envs = [Map.singleton acceleration (val (DoubleValue x)) | x <- vals]

prop_SpaceShip g =
--checkAssertions :: Double -> Duration -> Types -> Process -> Property
  --checkAssertions 0.1 1000 (Map.singleton acceleration (Continuous, Real (-10,10))) $
  checkAssertionsVal 0.1 100 (Map.singleton acceleration (Continuous, Real (-5,5))) $
    lower stdPrims $ simplify $ ship g & check

main = quickCheckWith stdArgs{ maxSuccess = 100000 } (prop_SpaceShip (1, 2, 0.5))

main' =
  checkAssertionsIO 0.1 100 (Map.singleton acceleration (Continuous, Real (-5,5))) $
    lower stdPrims $ simplify $ ship (1,2,0.5) & check

