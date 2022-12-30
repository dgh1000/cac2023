
import Cac.Pcs
import Util.Showable



-- 

main = do
  let motive = pFromList [0,1,3]
      frag = pFromList [1, 3, 5, 6, 10]
      motiveS = analyzeSubsets motive
      a = analyzeFrag motiveS frag
  putStrLn $ showiToString $ showI a
  


