-- A helicopter using model predictive control.
{-# LANGUAGE ScopedTypeVariables #-}
module HelicopterSimple where

import Process
import Process.Simplify
import Process.Language
import Process.Eval
import Data.Either
import Data.Generics.Uniplate.Data
import Utils
import qualified Data.Set as Set
import Data.List
import qualified Data.Map as Map
import Data.Functor.Identity
import Data.Maybe
import Debug.Trace
import Solver.FourierMotzkin hiding (var, trace, solve)
import qualified Solver.FourierMotzkin as FM

angle, speed, s :: Var
angle = Global "angle"
speed = Global "speed"
s = Global "s"

helicopter =
  continuous speed 0 (integral (integral (var angle)))
--  differentialEquation speed 0 $
--    transferFunction speed angle s helicopterTransfer

helicopterTransfer =
  (9.8 * (var s^2 - 0.5*var s + 6.3) /
    ((var s + 0.6565) * (var s^2 - 0.2366 * var s + 0.1493)))

testCase =
  [12,4,5,-2,3,1,5,2.5,6,4.5,8,7.5,10.25,9.75,12.5,12,14,13.5,15.25,14.75] ++
  replicate 50 16

test = simulateReal 0.01 (concat [replicate 60 (Map.singleton angle (val (DoubleValue (x/10000)))) | x <- testCase]) (lower stdPrims helicopter)
plotIt = plot "helicopter" 0.01 test

test' :: [Env Identity]
test' = take 6000 (control 0.01 speed angle 1 (lower stdPrims helicopter))
plotIt' = plot "helicopter" 0.01 test'

control :: forall f. (Valued f, Show (f Value), Ord (f Value)) => Double -> Var -> Var -> Double -> Process -> [Env f]
control delta output input setpoint p = go [env0]
  where
    env0 = execStep initial (start p)
    initial = Map.insert input (val (DoubleValue 0)) (emptyEnv delta)
    go envs@(env:_) =
      envs' ++ go (last envs':envs)
      where
        inp = controlled envs
        envs' = take (ceiling (tick / delta)) (tail (iterate (exec delta inp) env))

    exec :: Double -> f Double -> Env f -> Env f
    exec delta x env = execStep env' (step p)
      where
        env' = Map.insert input (vmap DoubleValue x) (Map.insert Delta (val (DoubleValue delta)) env)
    get var env = vmap doubleValue (fromJust (Map.lookup var env))

    tick = 0.01
    speed = 10
    scale = 0.001

    controlled :: [Env f] -> f Double
    controlled (env3:env2:env1:env0:_) =
      vlift7 predict x1 x2 x3 y0 y1 y2 y3
      where
        x1 = get input env1
        x2 = get input env2
        x3 = get input env3
        y0 = get output env0
        y1 = get output env1
        y2 = get output env2
        y3 = get output env3
    controlled xs = val (signum setpoint * scale * fromIntegral (length xs))

    predict :: Double -> Double -> Double -> Double -> Double -> Double -> Double -> Double
    predict x0 x1 x2 y0 y1 y2 y3 =
      case FM.solve prob of
        Nothing -> trace ("inconsistent: upping by " ++ show (signum err * scale)) $ x2 + signum err * scale
        Just sol ->
          let
            a = sol Map.! 'a'
            b = sol Map.! 'b'
            c = sol Map.! 'c'
          in
            if b == 0 then
              trace ("b: upping by " ++ show (signum err * scale)) $ x2 + signum err * scale
            else
              (reference err - fromRational a * y3 - fromRational c) / fromRational b
      where
        prob = problem [
          scalar (toRational y1) <== toRational y0 ^* FM.var 'a' + toRational x0 ^* FM.var 'b' + FM.var 'c',
          scalar (toRational y1) >== toRational y0 ^* FM.var 'a' + toRational x0 ^* FM.var 'b' + FM.var 'c',
          scalar (toRational y2) <== toRational y1 ^* FM.var 'a' + toRational x1 ^* FM.var 'b' + FM.var 'c',
          scalar (toRational y2) >== toRational y1 ^* FM.var 'a' + toRational x1 ^* FM.var 'b' + FM.var 'c',
          scalar (toRational y3) <== toRational y2 ^* FM.var 'a' + toRational x2 ^* FM.var 'b' + FM.var 'c',
          scalar (toRational y3) >== toRational y2 ^* FM.var 'a' + toRational x2 ^* FM.var 'b' + FM.var 'c']
          
        reference err =
          setpoint - exp (-tick / speed) * err

        err = setpoint - y3
