module Flowstar where

import Flowstar.Parse
import Flowstar.Translate
import Process.QuickCheck

checkFlowstar :: Double -> Double -> FilePath -> IO ()
checkFlowstar delta dur file = do
  mod <- parseFile file
  checkAssertionsIO delta dur (types mod) (model mod)
