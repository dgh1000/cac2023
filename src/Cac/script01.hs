
import Data.List(unfoldr)
import System.Random
import qualified Data.List as L

d1 = [(0,1),(1,2),(1,0),(0,-1)] :: [(Double,Double)]

divideListInTwo :: [a] -> ([a],[a])
divideListInTwo [] = ([],[])
divideListInTwo xs = (take l xs,drop l xs) where l = length xs `div` 2


main1 = print $ divideListInTwo ([] :: [Int])

main :: IO ()
main = do
  gen <- newStdGen
  let gs = take 4 $ unfoldr (Just . split) gen
      f :: StdGen -> Double
      f g = fst $ randomR (0,10) g
  print $ map f gs
  return ()


divideListN :: Int -> [a] -> [[a]]
divideListN n _ | n < 1 = error "divideListN called with n < 0"
divideListN 1 xs = [xs]
divideListN n xs = first : divideListN (n-1) last
  where
    (first,last) = splitAt (length xs `div` n) xs


main2 = divideListN 9 [0,1,2,3,4,6,5::Int]
