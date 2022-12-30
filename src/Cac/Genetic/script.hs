main = putStrLn $ show $
       foldl (\x y -> x*y/1000000) 1.0 [1000000,999999..999500]

