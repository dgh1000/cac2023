
import qualified Data.Map.Strict as M 
import qualified Data.ByteString as B
import Cac.Simpler.SimplerData
import Data.Serialize
import Cac.Simpler.Algo

opts1 = [ PitchOpt Nothing (Just (-6,-1))
        , PitchOpt Nothing (Just ( 3, 7)) ]

motives = M.fromList [(0,opts1)] 

ctx1 = Context motives 0 0 [] []

writeComps :: [Comp] -> IO ()
writeComps cs = do
  let s = encode cs
  B.writeFile "comp.dat" s
  
  


main = do
  let cs :: [Comp]
      cs = search ctx1
  writeComps cs
