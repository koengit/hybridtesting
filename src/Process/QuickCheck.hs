module Process.QuickCheck where

import Process.Eval
import Process.Input
import Process.Language
import Process.Plot
import Test.QuickCheck
import Utils
import qualified Data.Map as Map
import Data.Functor.Identity
import Optimize
import Data
import Val
--import Debug.Trace

--------------------------------------------------------------------------------
-- checking the assertions in a process

checkAssertionsVal :: Double -> Duration -> Types -> Process -> Property
checkAssertionsVal =
  checkAssertionsWith $ \input p ->
    let (input', tries, _) = forData input p in
    (input', tries)

checkAssertionsRandom :: Double -> Duration -> Types -> Process -> Property
checkAssertionsRandom =
  checkAssertionsWith (\input _ -> (input, 0))

checkAssertionsWith ::
  (Input -> (Input -> Double) -> (Input, Int)) ->
  Double -> Duration -> Types -> Process -> Property
checkAssertionsWith optimiser delta maxdur types p =
  forAll (genInput maxdur types) $ \input0 ->
    let (input', tries) = optimiser input0 (howTrue . run) in
      forAllShrink (return input') shrinkInput $ \input'' ->
        let envs  = simulateVal delta (sampleInput delta input'') p in
          whenFail (do plot "cex" delta envs
                       putStrLn ("used " ++ show tries ++ " steps in NM")) $
            the (run input'')
 where
  run input =
    let
      inps = sampleInput delta input
      envs = simulateVal delta inps p
      
      check pre post [] =
        (pre, post)
      
      check pre post (env:envs)
        | the pre' == False || the post' == False = (pre', post')
        | otherwise                               = check pre' post' envs
       where
        pre'  = pre  &&? (boolValue `mapVal` (env Map.! Pre))
        post' = post &&? (boolValue `mapVal` (env Map.! Post))

      (pre,post) = check (Val.val True) (Val.val True) envs
      
      ok = okRange input
    in (ok &&? pre) =>? post

  okRange (Input dur mp) =
    foldr (&&?) (Val.val True) $
    [ case types Map.! x of
        (_, Real (mn,mx))    -> (mn <=? v) &&? (v <=? mx)
        (_, Integer (mn,mx)) -> (fromIntegral mn <=? v) &&? (v <=? fromIntegral mx)
        _                    -> Val.val True
    | (x,(_,sig)) <- Map.toList mp
    , DoubleValue v <- values sig
    ]

--------------------------------------------------------------------------------

