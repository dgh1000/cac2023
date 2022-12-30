import Cac.SimplerD.Percentile

import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Map.Strict(Map)
import Data.Function


d :: Map String Double
d = M.fromList [ ("Nodin" , 9.0)
               , ("Nikki" , 8.9)
               , ("Robie2", 3.0)
               , ("Robie1", 3.0)
               , ("Robie2", 2.8)
               , ("Robie3", 2.9)
               , ("Foody" , 3.1) ]

-- do some search next

main = do
  let p = computePercentile d
      s = unlines $ map (showPerc id) $
          L.sortBy (compare `on` (getAtBelow . snd)) $ M.toList p
      bs = chooseBest 0.5 p
  putStrLn s
  putStrLn $ show bs
  

