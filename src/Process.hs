module Process(
  module Process,
  module Process.Language,
  module Process.Combinators,
  module Process.Eval,
  module Process.Simplify,
  module Process.Input) where

import Process.Language(Process, Step, Expr, PrimitiveKind(..), Var(..), vars)
import Process.Combinators
import Process.Eval(Value(..), Env, Valued(..), simulate, simulateReal, simulateVal)
import qualified Process.Eval
import Process.Simplify(lower, simplify)
import Process.Input(Types, Duration, Time(..), Type(..))
import Process.Pretty()
import qualified Plot
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Functor.Identity

plot :: FilePath -> Double -> [Env Identity] -> IO ()
plot name delta [] =
  Plot.plot name maxBound
    []
plot name delta envs@(env:_) =
  Plot.plot name maxBound
    [[(show x, xsys)]
    | x <- Map.keys env
    , case x of
        Delta   -> False
        --Local _ -> False
        _       -> True
    , let xsys = timed (map (value . runIdentity . Map.findWithDefault undefined x) envs)
    , not (null (fst xsys))
    ]
  where
    times    = [0, delta..]
    timed xs = unzip (filter (isNumber . snd) (zip times xs))
    
    value (BoolValue False) = 0
    value (BoolValue True)  = 1
    value (DoubleValue x)   = x
    
    isNumber x | x `elem` [1/0,(-1)/0] = False
               | otherwise             = True
    
