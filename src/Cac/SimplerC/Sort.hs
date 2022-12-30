module Cac.SimplerC.Sort where

import qualified Data.Map as M
import Data.Map(Map)
import Cac.SimplerC.Data
import Cac.SimplerC.Percentile


{-
groupedByStepId :: Results -> Map Integer [(EuResult,(Double,Double))]
rankGoodGroupedByStepId (Results byName byId) = error "jb98234a"
  where
    allNames = M.keys byName
    doName :: String -> Map Integer [EuResult]
    doNmae 
-}


-- <unit rank among unit voting on winning step only>
-- <winning step rank among steps voted on by unit>
data Rank = Rank PercentileData PercentileData


unitRankViaWinner :: Integer -> Results -> Map String (Double,Double)
unitRankViaWinner winId (Results byUName byStId _) = error "foo"
  where
    doUnit :: String -> Rank
    doUnit unitName = error "foo"
      where
        
