
import qualified Data.Map.Strict as M
import Control.Arrow
import Data.Map.Strict(Map)
import Cac.Simpler.Algo
import Cac.Simpler.SimplerData

nL = [Note 42 (Times 0 8) 4 "piano"]

mes = [MelodyElem (Just 0.9) (Just (1,36))] 

ns = M.fromList $ map ((tOn . nTimes) &&& pure) nL

-- 60. 

c1 = Comp ns (42,78) 0.1 24 96

out = nextPitches c1 mes 0

main = do
  putStrLn $ show out
 



