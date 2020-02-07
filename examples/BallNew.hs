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
  continuous x 0 (var x + delta * xv') &
  continuous y 0 (var y + delta * yv') &
  continuous xv (var xv0) xv' &
  continuous yv (var yv0) yv' &
  continuous collisions 0 (cond (xColl ||| yColl) (var collisions + 1) (var collisions))
  where
    xv' = cond xColl (- var xv) (var xv)
    yv' = cond yColl (- var yv) (var yv)
    x' = var x + delta * var xv
    y' = var y + delta * var yv

    yColl = nott (y' `between` (-5, 5))
    xColl = x' `between` (9, 10) &&& nott (y' `between` (1, 2))

check :: Process
check =
  process skip $
  assume "ball bounced through too soon"
    (nott (var collisions <=? 4 &&& var x >=? 15)) &
  assume "too many collisions"
    (nott (var collisions >=? 6 &&& var x <=? 15)) &
  assert "ball made it through after 5 collisions"
    (nott (var x >=? 15))
  
test :: (Show (f Bool), Valued f, Ord (f Value)) => [(Double, Double)] -> [Env f]
test vals = simulate 0.1 envs (lower stdPrims $ ball & check)
  where
    envs = [Map.fromList [(xv0, val (DoubleValue x)), (yv0, val (DoubleValue y))] | (x, y) <- vals]

prop_ball =
  checkAssertionsVal 0.1 10 (Map.fromList [(xv0, (Parameter, Real (-20,20))), (yv0, (Parameter, Real (-20,20)))]) $
    lower stdPrims $ simplify $ ball & check

main = quickCheckWith stdArgs{ maxSuccess = 100000 } prop_ball

main' =
  checkAssertionsIO 0.1 10 (Map.fromList [(xv0, (Parameter, Real (-200,200))), (yv0, (Parameter, Real (-200,200)))]) $
    lower stdPrims $ simplify $ ball & check

