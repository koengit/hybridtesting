module Flowstar where

import Flowstar.Parse
import Flowstar.Translate
import Process.QuickCheck
import Process.Simplify
import Test.QuickCheck

checkFlowstar :: Double -> Double -> FilePath -> IO ()
checkFlowstar delta dur file = do
  mod <- parseFile file
  checkAssertionsIO delta dur (types mod) (simplify (model mod))

checkFlowstarQC :: Double -> Double -> FilePath -> IO ()
checkFlowstarQC delta dur file = do
  mod <- parseFile file
  quickCheck (checkAssertionsRandom delta dur (types mod) (simplify (model mod)))

checkFlowstarNM :: Double -> Double -> FilePath -> IO ()
checkFlowstarNM delta dur file = do
  mod <- parseFile file
  quickCheck (checkAssertionsVal delta dur (types mod) (simplify (model mod)))
