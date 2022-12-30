import Cac.SimplerC.Percentile

import Text.Printf
import Data.Function
import qualified Data.List as L
import Data.Tuple
import qualified Data.Map as M

ls :: [(Double,String)]
ls = [ (0, "food")
     , (0, "other")
     , (1, "foo")
     , (3, "body")
     , (6, "panties")
     , (6, "nikki")
     , (98, "thingy")
     , (98, "jumpt")
     , (98, "jumper") ]


main = do
  let m = computePercentile $ M.fromList $ map swap ls
      ts = L.sortBy (comparePerc `on` snd) $ M.toList m
      showPair :: (String,Perc) -> String
      showPair (s,Perc d1 d2) =
        printf "%30s: below: %.3f at/below: %.3f" s d1 d2
  putStr $ unlines $ map showPair ts
