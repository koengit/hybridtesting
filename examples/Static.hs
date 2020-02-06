import Process
import qualified Data.Map as Map
import Process.QuickCheck
import Test.QuickCheck

y, u1, u2 :: Var
y  = Global "y"
u1 = Global "u1"
u2 = Global "u2"

types :: Types
types =
  Map.fromList
    [(u1, (Parameter, Real (0, 1))),
     (u2, (Parameter, Real (0, 1)))]

system :: Double -> Process
system thresh =
  continuous y
    (cond (var u1 >=? double thresh &&& var u2 >=? double thresh)
      (-2*(var u1 + var u2) - 5)
      (2*((var u1+1)^2 + (var u2+1)^2)))
    (var y)

check :: Process
check =
  loop (assert "y >= 0" (var y >=? 0))

test :: (Show (f Bool), Valued f) => Double -> (Double, Double) -> Env f
test thresh (x, y) = last $ simulate 1 envs (lower stdPrims $ system thresh & check)
  where
    envs = [Map.fromList [(u1, val (DoubleValue x)), (u2, val (DoubleValue y))]]

prop_Static thresh =
  checkAssertionsVal 1 1 types $
    lower stdPrims $ system thresh & check

main' thresh =
  checkAssertionsIO 1 1 types $
    lower stdPrims $ system thresh & check

