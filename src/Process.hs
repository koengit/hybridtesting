module Process(
  module Process,
  module Process.Plot,
  module Process.Language,
  module Process.Combinators,
  module Process.Eval,
  module Process.Simplify,
  module Process.Input) where

import Process.Language(Process, Stream, Expr, PrimitiveKind(..), Var(..), vars)
import Process.Combinators
import Process.Eval(Value(..), Env, Valued(..), simulate, simulateReal, simulateVal)
import qualified Process.Eval
import Process.Simplify(lower, simplify)
import Process.Input(Types, Duration, Shape(..), Type(..))
import Process.Pretty()
import Process.Plot(plot)

