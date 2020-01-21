import Process
import qualified Data.Map.Strict as Map

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
  sequential skip (var position >=? 100) $
    first (assert "reached destination" false)

test :: Valued f => [Double] -> [Env f]
test vals = simulate 1 envs (lower stdPrims $ ship & check)
  where
    envs = [Map.singleton acceleration (val (DoubleValue x)) | x <- vals]
