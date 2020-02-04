import Process
import qualified Data.Map as Map
import Process.QuickCheck
import Test.QuickCheck

x, y, xv, yv, xv0, yv0, collisions :: Var
x = Global "x"
y = Global "y"
xv = Global "xv"
yv = Global "yv"
xv0 = Global "xv0"
yv0 = Global "yv0"
collisions = Global "collisions"

ball :: Process
ball =
  continuous x (-20) (xIn 1) &
  continuous y 0 (yIn 1) &
  process (set yv (var yv0) & set xv (var xv0) & set collisions 0) updateVelocity
  where
    xIn k = var x + var xv * delta * k
    yIn k = var y + var yv * delta * k
    updateVelocity =
      ite (yIn 2 <=? -5 ||| yIn 2 >=? 5)
        (set yv (-var yv) & set collisions (var collisions+1))
        (ite (xIn 2 >=? 9 &&& xIn 2 <=? 10 &&& (yIn 2 <=? 1 ||| yIn 2 >=? 2))
          (set xv (-var xv) & set collisions (var collisions+1))
          skip)

check :: Process
check =
  process skip $
  assume "ball bounced through too soon"
    (nott (var collisions <=? 4 &&& var x >=? 15)) &
  assert "ball made it through after 5 collisions"
    (nott (var x >=? 15))
  
test :: (Show (f Bool), Valued f) => [(Double, Double)] -> [Env f]
test vals = simulate 0.1 envs (lower stdPrims $ ball & check)
  where
    envs = [Map.fromList [(xv0, val (DoubleValue x)), (yv0, val (DoubleValue y))] | (x, y) <- vals]

prop_ball =
  checkAssertionsVal 0.1 100 (Map.fromList [(xv0, (Parameter, Real (-20,20))), (yv0, (Parameter, Real (-20,20)))]) $
    lower stdPrims $ simplify $ ball & check

main = quickCheckWith stdArgs{ maxSuccess = 100000 } prop_ball
