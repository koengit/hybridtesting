module Process.QuickCheck where

import Process.Eval
import Process.Input
import Process.Language( Process, Var(..) )
import Process.Plot
import Test.QuickCheck
import Utils
import qualified Data.Map as Map
import Data.Functor.Identity
import Optimize
import Data
import Val
import System.Random
import OptimizeNew as O1
import Falsify

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
        pre'  = pre  &&+ (boolValue `mapVal` (env Map.! Pre))
        post' = post &&+ (boolValue `mapVal` (env Map.! Post))

      (pre,post) = check (Val.val True) (Val.val True) envs
      
      ok = okRange input
    in ok ==>? (pre ==>? post)

  okRange (Input dur mp) =
    foldr (&&?) (Val.val True) $
    [ case types Map.! x of
        (_, Real (mn,mx))    -> (mn <=? v) &&? (v <=? mx)
        (_, Integer (mn,mx)) -> (fromIntegral mn <=? v) &&? (v <=? fromIntegral mx)
        _                    -> Val.val True
    | (x,(_,sig)) <- Map.toList mp
    , DoubleValue v <- values sig
    ]

checkAssertionsIO :: Double -> Duration -> Types -> Process -> IO ()
checkAssertionsIO delta maxdur types p =
  do rnd <- newStdGen

     let go ((xs,r):xsrs) =
           do putStrLn (show inp ++ " --> " ++ show r)
              let envs = simulateVal delta (sampleInput delta inp) p
              plot "cex" delta envs
              if r < 0 then
                  do quickCheck $ forAllShrink (return inp) shrinkInput $ \inp' ->
                       let envs = simulateVal delta (sampleInput delta inp') p in
                         whenFail (plot "cex" delta envs) $
                           the (run inp')

                else
                  do go xsrs
          where
           inp = input xs
     
     go (optis rnd)
 where
  k = 5 -- max #points per signal

  maybe Nothing  = (-1)
  maybe (Just x) = x + eps

  eps = head . takeWhile (>0) . iterate (/2) $ 1

  optis rnd = progress $ falsifyBox {- O1.minimizeBox -} rnd (maybe . falsify . run . input) xsLR
   where
    xsLR = concat
           [ concat (replicate (points sh) [pt,dur]) ++ [pt]
           | (v,(sh,ty)) <- Map.toList types
           , let pt  = case ty of
                         Real (a,b)    -> (a,b)
                         Integer (a,b) -> (fromIntegral a, fromIntegral b)
                         Bool          -> (0,1)
                 dur = (0,maxdur)
           ]


  points Parameter = 0
  points _ = k

  progress []           = []
  progress ((xs,r):xrs) = (xs,r) : progress (dropWhile ((>=r).snd) xrs)

  input xs = Input maxdur sigs
   where
    group n = takeWhile (not . null) . map (take n) . iterate (drop n)
   
    sigs = Map.fromList (sigsFrom (Map.toList types) xs)
    sigsFrom [] [] = []
    sigsFrom ((v,(sh,ty)):vs) xs | length xs >= p =
      (v,(sh, sig ty (take p xs))):sigsFrom vs (drop p xs)
      where
        p = 2*points sh+1
  
    sig ty [x]      = End (val ty x)
    sig ty (x:d:xs) = Point (val ty x) d (sig ty xs)

    val (Real _)    x = DoubleValue x
    val (Integer _) x = DoubleValue (fromIntegral $ round x)
    val Bool        x = BoolValue (x >= 0.5)
 
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
        pre'  = pre  &&+ (boolValue `mapVal` (env Map.! Pre))
        post' = post &&+ (boolValue `mapVal` (env Map.! Post))

      (pre,post) = check (Val.val True) (Val.val True) envs
    in pre ==>? post

--------------------------------------------------------------------------------

