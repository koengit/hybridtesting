-- A helicopter using model predictive control.
{-# LANGUAGE ScopedTypeVariables #-}
module Helicopter where

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

angle, speed, s :: Var
angle = Global "angle"
speed = Global "speed"
s = Global "s"

helicopter =
  differentialEquation speed 0 $
    transferFunction speed angle s helicopterTransfer

helicopterTransfer =
  (9.8 * (var s^2 - 0.5*var s + 6.3) /
    ((var s + 0.6565) * (var s^2 - 0.2366 * var s + 0.1493)))

testCase =
  [12,4,5,-2,3,1,5,2.5,6,4.5,8,7.5,10.25,9.75,12.5,12,14,13.5,15.25,14.75] ++
  replicate 50 16

test = simulateReal 0.01 (concat [replicate 60 (Map.singleton angle (val (DoubleValue (x/10000)))) | x <- testCase]) (lower stdPrims helicopter)
plotIt = plot "helicopter" 0.01 test

test' = take 6000 (control 0.01 speed angle 1 (lower stdPrims helicopter))
plotIt' = plot "helicopter" 0.01 test'

control :: forall f. Valued f => Double -> Var -> Var -> Double -> Process -> [Env f]
control delta output input setpoint p = go env0
  where
    env0 = execStep initial (start p)
    initial = Map.insert input (val (DoubleValue 0)) (emptyEnv delta)
    go env = envs ++ go (last envs)
      where
        inp = controlled env
        envs = take (ceiling (tick / delta)) (tail (iterate (exec delta inp) env))

    exec :: Double -> f Double -> Env f -> Env f
    exec delta x env = execStep env' (step p)
      where
        env' = Map.insert input (vmap DoubleValue x) (Map.insert Delta (val (DoubleValue delta)) env)
    get var env = vmap doubleValue (fromJust (Map.lookup var env))

    tick = 0.6
    speed = 6
    horizon = 8

    controlled :: Env f -> f Double
    controlled env =
      vlift (+) (get input env) correction
      where
        correction, unitResponse, freeResponse, err :: f Double
        err = vlift (-) (val setpoint) (get output env)
        reference err =
          setpoint - exp (-fromIntegral horizon * tick / speed) * err
        freeResponse =
          get output $
            foldn horizon (exec tick (get input env)) env
        unitResponse =
          get output $
            foldn horizon (exec tick (val 1)) env
        correction =
          vlift3
            (\err freeResponse unitResponse ->
              (reference err - freeResponse) / unitResponse)
            err freeResponse unitResponse
