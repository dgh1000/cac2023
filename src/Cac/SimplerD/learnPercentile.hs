
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Map.Strict(Map)
import Text.Printf
import Data.Function
import Cac.SimplerD.Data
import Cac.SimplerD.Percentile


-- all tied
--
--   every one is 0 for below and 1 for at-below
--
--     if we are eliminating bad, we don't eliminate any
--
--     if we are choosing among good, we don't choose any
--
--   50% tied for highest, rest below
--
--     taking those with "below" of 50% or greater would take the half
--
--     so would taking those with 100%
--
--   66% tied for highest, rest below
--
--     take those with at-below of 100% would take tied top 2
--
--     none have a "below" of 50% or greater
--
--  reasoning: we certainly want every at-below = 100%.
--
--   66% tied for lowest
--
--     if we take every at-below=100%, we get 1. do we take more? the
--     remaining have below=0%. maybe don't want to take them. take the
--
--  if they aren't all tied, maybe we never want to take anything at
--  below=0%. Never take the dregs.
--
--   bottom 3/5 tied. one strategy could be take from x such that at-below(x)
--   >= 0.5, exluding those for which below(x)==0.0. This includes 40% of
--   items, a good number, and excludes tied for bottom.
--
--   a strategy could be the following:
--
--     call the set of all percentile scores P. Order P from greatest to
--     least. Call that L_P.
--
--     for s in L_P:
--
--        check the percentage of items included if we take all x such that
--        at-below(x) >= s
--
--        if this is less than 50%, continue
--
--        if this is more than 50%, consider whether to back up one
--
--   advance taking only > 50% . if we take all, then great. 
--
--   simple way: put the at-below percentage at 100%. Drop it and monitor
--   whether the number of items take is "acceptable" (say, between 40 and
--   60%). If none are acceptable then take all.
--

-- [(id, score)]
d1 :: [(Int,Double)]
d1 = [ (1,  1.5)
     , (2,  1.5)
     , (7,  1.6)
     , (8,  1.6)
     , (3,  1.7)
     , (4,  1.8)
     , (5,  1.8)
     , (10, 1.4)
     , (11, 1.3)
     , (12, 1.2) ]

d1m :: Map Int Double
d1m = M.fromList d1


main2 = do
  let m :: Map Int Perc
      m = computePercentile d1m
      t :: (Int,Perc) -> String
      t tt = showPerc show tt
  putStrLn $ unlines (map t $ M.toList m)
  let bs = chooseBest 0.5 m
  putStrLn $ unlines $ map show bs

  
main = do
  let m :: Map Int Perc
      m = computePercentile $ M.fromList d1
      f :: Int -> String
      f i = printf "%3d" i
      g (i,p) = showPerc f (i,p)
      xs :: [String]
      xs = map g $ L.sortBy (compare `on` (getAtBelow . snd)) $ M.toList m
  putStrLn $ unlines xs
