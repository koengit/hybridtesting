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
       [ "set terminal pdf enhanced font 'Times,18' lw 1"
       , "set grid"
       , "set output '" ++ file ++ ".pdf'"
       ] ++
       [ "plot 0 linewidth 1 linecolor black title '', "
      ++ "'" ++ file ++ "-" ++ name ++ ".xy' with lines title '" ++ name ++ "'"
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

--------------------------------------------------------------------------------

