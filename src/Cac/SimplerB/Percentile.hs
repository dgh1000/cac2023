{-# LANGUAGE RankNTypes, ScopedTypeVariables #-}

module Cac.SimplerB.Percentile where

import qualified Data.List as L
import Data.Function

data PercentileOut a = PercentileOut [(Double,[a])] [(Double,a)] 

showPercOut :: Show a => PercentileOut a -> String
showPercOut (PercentileOut thing1 thing2) =
  unlines $ map show thing1 ++ ["------------"] ++ map show thing2
    

percentile :: [a] -> (a -> Double) -> PercentileOut a
percentile items valueFn = PercentileOut z2 z1
  where
    lItems = fromIntegral $ length items
    -- s: items sorted
    s = L.sortBy (flip compare `on` valueFn) items
    -- gs: grouped by equality
    gs = L.groupBy (\i1 i2 -> valueFn i1 == valueFn i2) s
    -- ls: lengths mapped and accumulated
    ls = scanl (+) 0 $ map length gs
    -- grOut: groups in gs zipped with 'ls' and converted
    doGroup2 len gr = (lDouble,gr)
      where
        lDouble = 1 - fromIntegral len / lItems
    doGroup len gr = zip (repeat lDouble) gr
      where
        lDouble = 1 - fromIntegral len / lItems
    z1 = concat $ zipWith doGroup ls gs
    z2 = zipWith doGroup2 ls gs

lookupPercentile :: a -> PercentileOut a -> Double
lookupPercentile = error "foo"
    


data T = T Double
  deriving(Show)

t1 = [T 0, T (-1), T 4, T 0, T 4, T 5]

getT (T x) = x


main = putStrLn $ showPercOut $ percentile t1 getT


                     
