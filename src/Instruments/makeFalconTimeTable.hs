
import Text.Printf
import Util.Math

falconTimes :: [(Int,Double)]
falconTimes = [ (  0,     0.100)
              , (  1,     0.182)
              , (  2,     0.270)
              , (  3,     0.364)
              , (  4,     0.466)
              , (  5,     0.575)
              , (  6,     0.692)
              , (  7,     0.818)
              , (  8,     0.953)
              , ( 12,     1.60 )
              , ( 16,     2.47 )
              , ( 20,     3.62 )
              , ( 24,     5.16 )
              , ( 28,     7.21 )
              , ( 32,     9.94 )
              , ( 36,    13.6  )
              , ( 40,    18.4  )
              , ( 44,    24.9  )
              , ( 48,    33.5  )
              , ( 52,    44.9  )
              , ( 56,    60.2  )
              , ( 60,    80.6  )
              , ( 64,   108    )
              , ( 68,   144    )
              , ( 72,   192    )
              , ( 76,   256    )
              , ( 80,   342    )
              , ( 84,   456    )
              , ( 88,   608    )
              , ( 92,   810    )
              , ( 96,  1080    )
              , (100,  1440    )
              , (104,  1920    )
              , (108,  2560    )
              , (112,  3410    )
              , (116,  4540    )
              , (117,  4880    )
              , (118,  5240    )
              , (119,  5630    )
              , (120,  6050    )
              , (121,  6500    )
              , (122,  6980    )
              , (123,  7500    )
              , (124,  8060    )
              , (125,  8660    )
              , (126,  9310    )
              , (127, 10000    ) ] 

-- we want to loop over pairs

timePairs :: [((Int,Double),(Int,Double))]
timePairs = zip falconTimes $ tail falconTimes


p :: ((Int,Double),(Int,Double)) -> [(Int,Double)]
p ((i1,d1),(i2,d2)) = map h idxs
  where
    idxs :: [Int]
    idxs = [i1..i2-1]
    f :: Int -> Double
    f i = scale_3_2 (fromIntegral i1) (fromIntegral i) (fromIntegral i2)
                    0.0 1.0
    g :: Double -> Double
    g r = d1 * ((d2/d1)**r)
    h i = (i,g $ f i)

output :: [(Int,Double)] -> IO ()
output xs = do
  let f :: (Int,Double) -> String
      f (i,d) = printf ", (%d,%10.3f)\n" i d
      s = concatMap f xs
  writeFile "falconTimingTable.txt" s


main = do
  -- print [1..1]
  -- let r = take 10 $ repeat (1,10.0)
  let xs = concatMap p timePairs
      -- fs = filter (\(i,_) -> i `mod` 4 == 0) xs
  output xs

main2 = do
  print $ 1247.077/1160.536
  print $ 1160.536/1080
  
