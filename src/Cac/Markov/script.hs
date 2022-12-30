


xs :: [Int]
xs = [3, 4]


combine :: Int -> Int -> Either String Int
combine x y
  | x `mod` 2 == 0 && y `mod` 2 == 0 = Right $ x + y
  | otherwise                        = Left "foo"


combineMany :: [Int] -> Either String Int
combineMany ints = hs
  where
    gs :: [(Int -> Either String Int)]
    gs = map combine ints
    hs = foldl (>>=) (Right 0) gs
    -- g = foldl (\g h -> combine g h  (Right 0)



v = Right 2 >>= combine 4 >>= combine 4 >>= combine 2


main = do
  putStrLn $ show v
  
