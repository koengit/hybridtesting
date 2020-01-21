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
plot name delta envs@(env:_) =
  Plot.plot name maxBound
    [[(x, timed (map (value . runIdentity . Map.findWithDefault undefined (Global x)) envs))]
    | Global x <- Map.keys env ]
  where
    times = [0, delta..]
    timed xs = unzip (zip times xs)
    value (BoolValue False) = 0
    value (BoolValue True) = 1
    value (DoubleValue x) = x
