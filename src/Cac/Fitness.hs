module Cac.Fitness where

import qualified Data.Map as M
import qualified Data.List as L
import Control.Arrow
import Data.Array.IArray
import Cac.Comp


data FitConfig = FitConfig
  { fcLoNote :: Int
  , fcHiNote :: Int
  }


data DFitConfig = DFitConfig
  { dfcNTake   :: Int
  , dfcShape   :: [(Int,Int)]
  , dfcNubFlag :: Bool
  }
  

rmsDFit :: Array Int Int -> Double
rmsDFit arr = sum diffsSquared / lenInput
  where
    as :: [Double]
    as = map (fromIntegral . snd) $ assocs arr
    lenInput = fromIntegral $ length as
    avg | lenInput > 0 = sum as / lenInput
    diffs = map (\x -> abs (x-avg)) as
    diffsSquared = map (\x -> x*x) diffs


-- dFit2
--
--   Compute distribution fitness
dFit2 :: FitConfig -> Comp -> Double
dFit2 = error "foo"

dFit :: Bool -> [(Int,Int)] -> Int -> Int -> [Int] -> Array Int Int
dFit nubFlag shape lo hi input = foldr f init $ L.nub input
  where
    -- we need to make a "hump", using shape, from every entry in the input
    -- list. don't repeat them
    lenShape = length shape
    (minShapeIdx, maxShapeIdx) | lenShape > 0 =
       (minimum $ map fst shape, maximum $ map fst shape)
    nubbed = L.nub input
    xs :: Array Int Int
    xs = array (minShapeIdx,maxShapeIdx) shape
    -- f will take a value 'x' in the input, and current state of accumulator
    -- array, shift the shape to be centered at 'x', and add it to the
    -- accumulator
    init :: Array Int Int
    init = array (lo,hi) $ zip [lo..hi] (repeat 0)
    f :: Int -> Array Int Int -> Array Int Int
    f value acc = accum (+) acc $ map (first (+ value)) shape



fit1 :: Int -> Comp -> Double
fit1 nTake (Comp notes) = error "foo"
  where
    ns = take nTake $ concatMap snd $ M.toDescList notes


main = do
  let x = dFit True [(-1,1),(0,1),(1,1)] (-4) 4 [0,0,1,3]
      r = rmsDFit x
  putStrLn $ show $ x
  putStrLn $ show r
