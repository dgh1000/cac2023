
import qualified Data.ByteString as B
import Text.Printf
import Data.Serialize
import Util.Exception
import Cac.Simpler.SimplerData

main = do
  b <- B.readFile "comp.dat"
  let ds :: [Comp]
      ds = case decode b of
        Left err -> throwMine $ printf "error decoding: %s" err
        Right x  -> x
  putStrLn $ printf "length: %d" $ length ds
