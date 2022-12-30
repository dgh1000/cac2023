

f :: Int -> Double
f i = (1600000 - fromIntegral i)/1600000

h :: [Double]
h = map f [1..300]


main = putStrLn $ show $ product h
