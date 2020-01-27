module Process.QuickCheck where

import Process
import Process.Input
import Utils
import Test.QuickCheck
import System.Random
import Control.Monad
import qualified Data.Map as Map
import Data.Functor.Identity
import Optimize
import Data
import Val
import VBool
import Badness
import Debug.Trace

----------------------------------------------------------------------
-- Generation.
----------------------------------------------------------------------

genValue :: Type -> Gen Value
genValue (Real (x, y)) =
  DoubleValue <$> clamp <$> choose (x - delta, y + delta)
  where
    delta = (y - x) / 10
    clamp z
      | z < x = x
      | z > y = y
      | otherwise = z
genValue (Integer (x, y)) = DoubleValue <$> fromInteger <$> choose (x, y)
genValue Bool = BoolValue <$> arbitrary

genSignal :: Gen Duration -> (Time, Type) -> Gen Signal
genSignal gen (Continuous, ty) =
  infiniteListOf $ liftM2 (,) gen $
    oneof [
      Linear <$> liftM2 (,) arbitrary arbitrary,
      Constant <$> DoubleValue <$> arbitrary ]
genSignal gen (Discrete, ty) =
  infiniteListOf $ liftM2 (,) gen $
    Constant <$> genValue ty
genSignal _ (Parameter, ty) = do
  x <- genValue ty
  return [(1/0, Constant x)]

genInput :: Duration -> Types -> Gen Input
genInput maxdur types = do
  dur <- sized $ \n -> choose (0, maxdur*fromIntegral n/100)
  let gen = (^2) <$> choose (0, sqrt dur)
  fmap (Input dur) $ mapM (\ty -> cut dur <$> genSignal gen ty) types

----------------------------------------------------------------------
-- Shrinking.
----------------------------------------------------------------------

shrinkElems :: (a -> [a]) -> [a] -> [[a]]
shrinkElems _ [] = []
shrinkElems shr (x:xs) =
  [x':xs | x' <- shr x] ++
  [x:xs' | xs' <- shrinkElems shr xs]

shrinkValue :: Type -> Value -> [Value]
shrinkValue (Real (lo, hi)) (DoubleValue x) =
  [ DoubleValue y | y <- shrink x, lo <= y && y <= hi ]
shrinkValue (Integer (lo, hi)) (DoubleValue x) =
  [ DoubleValue (fromInteger y) | y <- shrink (truncate x), lo <= y && y <= hi ]
shrinkValue _ (BoolValue x) = map BoolValue (shrink x)

shrinkPiece :: Type -> Piece -> [Piece]
shrinkPiece ty (Linear (x, y)) =
  Constant (DoubleValue x):
  Constant (DoubleValue y):
  [ Constant (DoubleValue ((x+y)/2)) | not (isInteger ty) ] ++
  [ Linear (x', y) | DoubleValue x' <- shrinkValue ty (DoubleValue x) ] ++
  [ Linear (x, y') | DoubleValue y' <- shrinkValue ty (DoubleValue y) ]
  where
    isInteger (Integer _) = True
    isInteger _ = False
shrinkPiece ty (Constant x) =
  map Constant (shrinkValue ty x)

shrinkSignal :: Type -> Signal -> [Signal]
shrinkSignal ty xs =
  merge xs ++ shrinkElems (liftShrink2 shrink (shrinkPiece ty)) xs
  where
    merge (x:y:xs) =
      merging x y xs ++
      map (x:) (merge (y:xs))
    merge _ = []
    merging (t,x) (u,y) xs =
      [(t+u, x):xs,
       (t+u, y):xs] ++
      [(t+u, Linear (x', y')):xs
      | Just (x', _) <- [bounds x],
        Just (y', _) <- [bounds y]]
    bounds (Linear (x, y)) = Just (x, y)
    bounds (Constant (DoubleValue x)) = Just (x, x)
    bounds _ = Nothing

shrinkInput :: Types -> Input -> [Input]
shrinkInput tys (Input dur xs) =
  [ Input dur' (Map.map (cut dur') xs) | dur' <- shrink dur ++ [dur-1, dur / 2], dur' > 0, dur' < dur ] ++
  [ Input dur ys | ys <- shr xs, xs /= ys ]
  where
    shr =
      map Map.fromList .
      shrinkElems
        (\(x, v) ->
            [ (x, recut dur v')
            | v' <- shrinkSignal (snd (Map.findWithDefault undefined x tys)) v]) .
      Map.toList

----------------------------------------------------------------------
-- Testing.
----------------------------------------------------------------------

checkAssertions :: Double -> Duration -> Types -> Process -> Property
checkAssertions delta maxdur types p =
  forAllShrink (genInput maxdur types) (shrinkInput types) $ \input ->
    let
      inps  = sampleInput delta input
      envs  = map (Map.map Val.the) (simulateVal delta inps p)
      --envs  = map (Map.map runIdentity) (simulateReal delta inps p)
      envs' = takeUntil (boolValue . (Map.! Post)) envs
      
      final = last envs'
      pre   = boolValue $ final Map.! Pre
      post  = boolValue $ final Map.! Post
    in
      counterexample (show inps) $
      not (null envs') Test.QuickCheck.==>
      --counterexample ("After " ++ show (fromIntegral (length envs') * delta) ++ "s:") $
      --flip (foldr counterexample)
      --  [ "  " ++ show x ++ " = " ++ show v | (x, v) <- Map.toList final ] $
      --counterexample "Postcondition failed." $
        pre Test.QuickCheck.==> post

checkAssertionsVal :: Double -> Duration -> Types -> Process -> Property
checkAssertionsVal delta maxdur types p =
  withBadness $
  forAll (genInput maxdur types) $ \input0 ->
  --let input0 = Input 100 (Map.singleton (Global "acceleration") sig)
  --    sig    = [(4.5,Constant (DoubleValue 5)),(11,Constant (DoubleValue (-5))),(16,Constant (DoubleValue 5))]
  --in 
    let (input', result') = forData input0 run in
      forAllShrink (return input') (shrinkInput types) $ \input'' ->
        let envs  = simulateVal delta (sampleInput delta input'') p
            envs' = [ Map.map (return . DoubleValue . conv) env | env <- envs ]
            
            conv v = case (wf v, the v) of
                       (False, _)         -> 0
                       (_, DoubleValue x) -> x
                       (_, BoolValue _)   -> VBool.howTrue (propVal (boolValue `vmap` v))
         in whenFail (plot "cex" delta envs') $
              run input''
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
    in propVal (Val.nott (okRange input &&? pre) ||? post)

  okRange (Input dur mp) =
    dur >? 0 &&? foldr (&&?) (Val.val True)
    [ case types Map.! x of
        (_, Real (mn,mx))    -> (mn Val.<=? v) &&? (v Val.<=? mx)
        (_, Integer (mn,mx)) -> (fromIntegral mn Val.<=? v) &&? (v Val.<=? fromIntegral mx)
        _                    -> Val.val True
    | (x,ps) <- Map.toList mp
    , (_,p) <- ps
    , v <- case p of
             Linear (a,b)             -> [a,b]
             Constant (DoubleValue a) -> [a]
             _                        -> []
    ]

