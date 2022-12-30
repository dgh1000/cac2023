k
{-# LANGUAGE ScopedTypeVariables #-}

module Cac.SimplerC.Percentile where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Map(Map)
import Data.Function
import Data.Tuple

-- we need to see if we have moved the script into this yet

-- <percent at or below> <percent below>
data Perc = Perc Double Double
          deriving(Show)

comparePerc :: Perc -> Perc -> Ordering
comparePerc (Perc d1 _) (Perc d2 _) = compare d1 d2

computePercentile :: forall a. Ord a => Map a Double -> Map a Perc
computePercentile mapIn = M.fromList $ zipWith fg2 fg6 fg7
  where
    nIn = length mapIn
    
    lGroups :: [ [(Double,a)] ]
    lGroups = L.groupBy ((==) `on` fst) . L.sortBy (compare `on` fst) $
              map swap $ M.toAscList mapIn

    lGroupLenAccum :: [Int]
    lGroupLenAccum = scanl (+) 0 $ map length lGroups
    lPercentile :: [Double]
    lPercentile = map ((/fromIntegral nIn) . fromIntegral) lGroupLenAccum

    -- fg will take a group list rank together with a group list of items, and
    -- zip and explode them.
    fg :: [Double] -> [[a]] -> [(a,Double)]
    fg ds ss = concat $ zipWith fg3 ds ss

    fg5 :: [[a]]
    fg5 = map (map snd) lGroups

    fg6 = fg lPercentile fg5
    fg7 = fg (tail lPercentile) fg5

    fg2 :: (a,Double) -> (a,Double) -> (a,Perc)
    fg2 (s1,d1) (s2,d2) | s1 == s2 = (s1,Perc d1 d2)
    
    fg3 :: Double -> [a] -> [(a,Double)] 
    fg3  r ss = zip ss (repeat r)

    fg4 :: [[a]]
    fg4 = map (map snd) lGroups



{-
data PercentileOut a = PercentileOut [(Double,[a])] [(Double,a)] 

showPercOut :: Show a => PercentileOut a -> String
showPercOut (PercentileOut thing1 thing2) =
  unlines $ map show thing1 ++ ["------------"] ++ map show thing2
-}



