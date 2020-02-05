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
  define x (continuous 0 (var x + delta * xv')) &
  define y (continuous 0 (var y + delta * yv')) &
  define xv (continuous (var xv0) (cond xColl (var xv) (- var xv))) &
  define yv (continuous (var yv0) (cond yColl (var yv) (- var yv))) &
  define collisions (continuous 0 (cond (xColl ||| yColl) (var collisions + 1) (var collisions)))
  where
    xv' = cond xColl (var xv) (- var xv)
    yv' = cond yColl (var yv) (- var yv)
    x' = var x + delta * var xv
    y' = var x + delta * var yv

    yColl = nott (y' `between` (-5, 5))
    xColl = x' `between` (9, 10) &&& nott (y' `between` (1, 2))

check :: Process
check =
  assume "ball bounced through too soon"
    (continuous true (nott (var collisions <=? 4 &&& var x >=? 15))) &
  assert "ball made it through after 5 collisions"
    (continuous true (nott (var x >=? 15)))
  
test :: (Show (f Bool), Valued f) => [(Double, Double)] -> [Env f]
test vals = simulate 0.1 envs (lower stdPrims $ ball & check)
  where
    envs = [Map.fromList [(xv0, val (DoubleValue x)), (yv0, val (DoubleValue y))] | (x, y) <- vals]

prop_ball =
  checkAssertionsVal 0.1 100 (Map.fromList [(xv0, (Parameter, Real (-20,20))), (yv0, (Parameter, Real (-20,20)))]) $
    lower stdPrims $ simplify $ ball & check

main = quickCheckWith stdArgs{ maxSuccess = 100000 } prop_ball
