{-# LANGUAGE ScopedTypeVariables #-}

module Cac.SimplerD.Percentile where

import qualified Data.Map.Strict as M
import qualified Data.List as L
import Text.Printf
import Control.Arrow 
import Data.Map(Map)
import Data.Function
import Data.Tuple

-- next goal
--
--   add code for selecting the "best answers" or "worst answers" by some
--   algorithm
--
--

-- chooseBest
--
-- This algorithm chooses a "cutoff percentile" C and filters through any
-- item with a percentile p such that p >= C.
--
-- It's configured with two values, P_max and P_min. P_max refers to the
-- maximum C can be (enforcing that a minimum proportion of the items must
-- make it through). P_min refers to the smallest C can be.
--
-- Our algorithm picks the largest C that meets these two criteria. If there
-- is no such C, then it picks the largest C that is less than P_min.
--  
-- Algorithm
--
--   - group into ties by at-below percentile P: [(<P>,[a])]. sort descending
--     by P
--
--   - create indices meeting P_max criteria. call smallest one I_P_max.
--
--   - create indices meeting P_min criteria. call largest one I_P_min.
--
--
--   - cases: (note, given that P_min < 1, then I_P_min must be defined
--
--      - I_P_max not defined: take all
--
--      - I_P_max <= I_P_min: use I_P_max as cutoff index
--
--      - I_C_max > I_P_min: 
--
--      - I_C_max >= I_C_min: set I_take to I_C_max (assert this is less than
--        list length)
--
--      - I_C_max < I_C_min: set I_take to I_C_min (assert this is less than
--        list length)
--



-- chooseBest
--
-- This algorithm chooses a "cutoff percentile" C and filters through any
-- item with a percentile p such that p >= C.
--
-- Note: we always use "at below" percentile.
--
-- It's configured with P_max, which refers to the maximum C can be (thereby
-- enforcing that a minimum proportion of the items must make it
-- through).
--
-- Call the set of percentiles which actually occur in items in the data AP.
-- Our algorithm picks the largest value of AP, AP_i such that AP_i <=
-- P_max. If there is no such value, then we just take all the data.
--  
-- Algorithm
--
--   - group into ties by at-below percentile P: [(<P>,[a])]. sort descending
--     by P
--
--   - take from list while P > P_max. if not all are taken, take one more.
--     if none are taken, take all.
--
chooseBest :: forall a. Double -> Map a Perc -> [a]
chooseBest maxPerc m
  | 1.0 > maxPerc && maxPerc > 0 = concatMap f3 list5
  where
    list :: [(a,Perc)]
    list = M.toList m
    list2 :: [(Double,a)]
    list2 = map (\(item,p) -> (getAtBelow p,item)) list
    list2s :: [(Double,a)]
    list2s = reverse $ L.sortBy (compare `on` fst) list2
    list3 :: [[(Double,a)]]
    list3 = L.groupBy (\(x,_) (y,_) -> x == y) list2s
    f2 :: [(Double,a)] -> (Double,[a])
    f2 xs@(x:_) = (fst x,map snd xs)
    list4 :: [(Double,[a])]
    list4 = map f2 list3
    pred1 :: (Double,[a]) -> Bool
    pred1 (x,_) = x > maxPerc
    list5 :: [(Double,[a])]
    list5 = takeWhilePlusOne pred1 list4
    f3 :: (Double,[a]) -> [a]
    f3 = snd
    
{-    
chooseBestDebug :: forall a. Double -> Map a Perc -> [[(Double,a)]]
chooseBestDebug maxPerc m
  | 1.0 > maxPerc && maxPerc > 0 = list3
  where
    list :: [(a,Perc)]
    list = M.toList m
    list2 :: [(Double,a)]
    list2 = map (\(item,p) -> (getAtBelow p,item)) list
    list2s :: [(Double,a)]
    list2s = reverse $ L.sortBy (compare `on` fst) list2
    list3 :: [[(Double,a)]]
    list3 = L.groupBy (\(x,_) (y,_) -> x == y) list2s
-}

{-
-- list is sorted decreasing order by good percentage. 
chooseBest' :: Double -> [(a,Double)] -> [a]
chooseBest' maxPerc list = error "foo"
  where
-}    


takeWhilePlusOne :: (a -> Bool) -> [a] -> [a]
takeWhilePlusOne g xs = l3 ++ take 1 l4
  where
    l3 = takeWhile g xs
    l4 = dropWhile g xs


chooseBest_matchingIndices :: (Double -> Bool) -> [(Double,a)] -> [Int]
chooseBest_matchingIndices g = L.findIndices (\(d,_) -> g d)


-- <percent below> <percent at or below>
data Perc = Perc Double Double
          deriving(Show)

showPerc :: (a -> String) -> (a,Perc) -> String
showPerc f (i,Perc atBelow below) =
  printf "%s below: %8.3f at-below: %8.3f" (f i) atBelow below


getAtBelow :: Perc -> Double
getAtBelow (Perc _ g) = g


getBelow :: Perc -> Double
getBelow (Perc b _) = b


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





