module Process.Plot where

import Process.Eval
import Process.Language( Var(Delta) )
import qualified Data.Map as M
import System.Process( system )

--------------------------------------------------------------------------------
-- plot

plot :: Valued f => FilePath -> Double -> [Env f] -> IO ()
plot file delta envs =
  do -- creating .xy files
     sequence_
       [ writeFile (file ++ "-" ++ name ++ ".xy") $ unlines $ xys
       | (name, xys) <- tabs
       ]

     -- creating the gnuplot script
     writeFile (file ++ ".in") $ unlines $
       [ "set terminal pdf enhanced font 'Times,18' lw 3"
       , "set grid"
       , "set output '" ++ file ++ ".pdf'"
       ] ++
       [ "plot '" ++ file ++ "-" ++ name ++ ".xy' with lines title '" ++ name ++ "'"
       | (name,_) <- tabs
       ]

     -- running gnuplot
     system ("gnuplot < '" ++ file ++ ".in'")
     return ()
  where
   times = [0, delta..]

   tabs =
     [ (show v, xys)
     | env <- take 1 envs
     , v <- M.keys env
     , case v of
         Delta -> False
         _     -> True
     , let xys = [ show t ++ " " ++ show y
                 | (t,x) <- times `zip` map (M.! v) envs
                 , let y = vplot x
                 , isNumber y
                 ]
     , not (null xys)
     ]

   isNumber x | x `elem` [1/0,(-1)/0] = False
              | otherwise             = True

{-
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

plot :: FilePath -> Int -> [[(String,(S Double,S Double))]] -> IO ()
plot file n xyss0 =
  do let xyss :: [[(String,(S Double,S Double))]]
         xyss = map (map (id *** (take n *** take n))) xyss0
     dir <- getCurrentDirectory
     sequence_
       [ writeFile (dir </> file ++ "_" ++ name ++ "_.xy") $ unlines $
           [ show x ++ " " ++ show y
           | (x,y) <- take n (uncurry zip xy)
           ]
       | xys <- xyss,
         (name,xy) <- xys
       ]
     writeFile (dir </> file ++ "_gnuplot_.in") $ unlines $
       [ "set terminal pdf enhanced font 'Times,18' lw 3"
       , "set grid"
       , if any (< 0) (concatMap (snd . snd) . concat $ xyss) then "" else "set yrange [0:]"
       , "set output '" ++ file ++ "_plot_.pdf'" ] ++
       [ "plot " ++
         intercalate ","
           [ "'" ++ file ++ "_" ++ name ++ "_.xy' with lines title '" ++ name ++ "'"
           | (name, xy) <- xys ]
       | xys <- xyss ]
     system ("gnuplot < \"" ++ (dir </> file ++ "_gnuplot_.in\""))
     return ()
-}

--------------------------------------------------------------------------------

