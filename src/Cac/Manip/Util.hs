
module Cac.Manip.Util where

import qualified Data.List as L
import Data.Function
import Control.Arrow

fromPcRegister :: Int -> Double -> Int
fromPcRegister pc range = L.minimumBy (compare `on` g) xs
  where
    p1 = pc + 12 * round ((range - fromIntegral pc) / 12)
    xs = [p1-12,p1,p1+12]
    g pIn = abs $ fromIntegral pIn - range

chooseM :: Ord a => Int -> [a] -> [[a]]
chooseM n xs = L.nub $ map (L.sort . take n) $ L.permutations xs

test1 = do
  mapM_ (print . (id &&& fromPcRegister 0)) [36.1,39.1..90]
