
import qualified Data.Map as M
import Control.Arrow
import Data.Map(Map)
import qualified Data.ByteString as B
import Data.Set(Set)
import Cac.Pcs
import Data.Serialize

mkMap :: Map Int (Set Pcs) -> Map Pcs (Map Int [Pcs])
mkMap subsets = M.fromList xs
  where
    xs = map (id &&& (analyzeFrag subsets . fromIntegral)) [1..4095]


main = do
  let pcs = pFromList [0,1,6,8]
      m = mkMap (analyzeSubsets pcs)
      s = encode m
  B.writeFile "0168.pc" s
   
