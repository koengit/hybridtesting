-- Automatic transmission, ported from Matlab.
module Transmission where

import Process
import qualified Data.Map.Strict as Map

-- input variables
throttle, brakeTorque :: Var
throttle = Global "throttle"
brakeTorque = Global "brakeTorque"

-- input types
types :: Types
types =
  Map.fromList
    [(throttle, (Continuous, Real (0, 100))),
     (brakeTorque, (Continuous, Real (-1, 1)))]

-- n.b. brake goes from -1 to 1

-- output variables
rpm, speed, gear :: Var
rpm = Global "rpm"
speed = Global "speed"
gear = Global "gear"

-- state variables
outputTorque, impellerTorque, transmissionRPM :: Var
outputTorque = Global "outputTorque"
impellerTorque = Global "impellerTorque"
transmissionRPM = Global "transmissionRPM"

-- the system
--system = engine & shiftLogic & transmission & vehicle

lei :: Double
lei = 0.0219914882835559

engine :: Process
engine =
  continuous rpm 0 (clampedIntegral (-400) 5000 e)
  where
    e = (engineTorque - var impellerTorque)/double lei
    engineTorque = interpolate2d xs ys table (var impellerTorque, var throttle)

    xs = [0,20,30,40,50,60,70,80,90,100]
    ys = [800,1200,1600,2000,2400,2800,3200,3600,4000,4400,4800]
    table = [[-40,-44,-49,-53,-57,-61,-65,-70,-74,-78,-82],[215,117,85,66,44,29,10,-2,-13,-22,-32],[245,208,178,148,122,104,85,66,48,33,18],[264,260,241,219,193,167,152,133,119,96,85],[264,279,282,275,260,238,223,208,189,171,152],[267,290,293,297,290,275,260,256,234,212,193],[267,297,305,305,305,301,293,282,267,249,226],[267,301,308,312,319,323,319,316,297,279,253],[267,301,312,319,327,327,327,327,312,293,267],[267,301,312,319,327,334,334,334,319,305,275]]
