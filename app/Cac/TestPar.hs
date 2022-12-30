{-# LANGUAGE BangPatterns #-}

import qualified Data.List as L
import Text.Printf
import System.CPUTime
import Data.Function
import Control.Parallel
import Control.Parallel.Strategies
import Control.DeepSeq


{-
search1 :: Int -> ([Double] -> Double) -> ([Double] -> [Double]) -> [Double] -> (Double,[Double])
search1 finalSize evalFunc stepFunc listIn
  | length listIn == finalSize = (evalFunc listIn,listIn)
  | otherwise = 
      let steps = stepFunc listIn
          f s = search1 finalSize evalFunc stepFunc $ s:listIn
      in L.maximumBy (compare `on` fst) $ map f steps 
-}

search1_par :: Int ->
  ([Double] -> Double) -> ([Double] -> [Double]) -> [Double] -> (Double,[Double])
search1_par finalSize evalFunc stepFunc !listIn
  | length listIn == finalSize = (evalFunc listIn,listIn)
  | otherwise = runEval $ do
      let steps = stepFunc listIn
          (steps1,steps2) = divideListInTwo steps
          f !s = search1_par finalSize evalFunc stepFunc $ s:listIn
      results1 <- rpar $ force $ map f steps1
      results2 <- rseq $ force $ map f steps2
      return $ L.maximumBy (compare `on` fst) (results1++results2)
      --     results = force results1 `par` (force results2 `pseq` (results1++results2))
      -- in L.maximumBy (compare `on` fst) results


{-
force :: [a] -> ()
force xs = go xs `pseq` ()
  where go (x:xs) = x `pseq` go xs
        go [] = 1
-}


divideListInTwo :: [a] -> ([a],[a])
divideListInTwo [] = ([],[])
divideListInTwo !xs = (take l xs,drop l xs) where l = length xs `div` 2

eval1 :: [Double] -> Double
eval1 xs = v1 - v2 + v3
  where
    !v1 = sum $ zipWith (*) (cycle [1]) xs
    !v2 = sum . map (*2) $ zipWith (*) (cycle [1,0]) xs
    !v3 = sum . map (*3)  $ zipWith (*) (cycle [1,0,0]) xs


step1 :: [Double] -> [Double]
step1 xs | l == 0 = take 8 $ xs
         | l == 1 = take 8 $ map (/2) xs
         | l == 2 = take 8 $ map (*3) xs
  where
    !l = length xs `mod` 3


main = do
  t1 <- getCPUTime
  let f :: Double -> String
      f x = printf "%5.1f" x
      (_,result) = search1_par 13 eval1 step1 [1,2,3]
  putStrLn $ concatMap f result
  t2 <- getCPUTime
  putStrLn $ printf "CPU time: %.3f" ((fromIntegral $ t2-t1) / 1000000000000 :: Double)
