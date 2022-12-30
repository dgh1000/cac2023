
import Util.Math(scale_3_2)

main = do
  let x1 = log 5420
      x2 = log 20000
      v1 = 103
      v2 = 127
      f vIn = exp $ scale_3_2 v1 (fromIntegral vIn)  v2 x1 x2
  putStrLn $ unlines $ map (show . f) [120..127]
      
