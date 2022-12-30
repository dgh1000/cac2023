
import qualified Data.List as L
import Data.Function
import Text.Printf
import Debug.Trace

-- LEFT OFF: implementing percentile test. need to flip key and score in
-- 'ls'. in 'main' the zip action pairs score with sub-group of elements, but
-- we need to explode that subgroup ( i.e. (a,[b]) -> [(a,b)] ). also need to
-- pair with percentile value : [(c,b)]

func :: [(String,Int)] -> [(String,Double)]
func xs = error "foo"
  where
    xss = L.groupBy ((==) `on` snd) . L.sortBy (compare `on` snd) $ xs


explode :: (a,[b]) -> [(a,b)]
explode (x,ys) = zip (repeat x) ys 


data Perc = Perc Double Double

-- LEFT OFF: going to combine existing routines in this one. generate absolute
-- rank for each string element. Leave off score. Keep open whether we define
-- percentile as "at or below" or "below"
percentileTest :: [(Double,String)] -> [(String,Perc)]
percentileTest lIn = zipWith fg2 fg6 fg7
  where
    nIn = length lIn
    lGroups :: [ [(Double,String)] ]
    lGroups = L.groupBy ((==) `on` fst) . L.sortBy (compare `on` fst) $ lIn
    lGroupLengths = map length lGroups
    lGroupLenAccum :: [Int]
    lGroupLenAccum = scanl (+) 0 lGroupLengths
    lPercentile :: [Double]
    lPercentile = map ((/fromIntegral nIn) . fromIntegral) lGroupLenAccum

    -- fg will take a group list rank together with a group list of items, and
    -- zip and explode them.
    fg :: [Double] -> [[String]] -> [(String,Double)]
    fg ds ss = concat $ zipWith fg3 ds ss

    fg5 :: [[String]]
    fg5 = map (map snd) lGroups

    fg6 = fg lPercentile fg5
    fg7 = fg (tail lPercentile) fg5

    fg2 :: (String,Double) -> (String,Double) -> (String,Perc)
    fg2 (s1,d1) (s2,d2) | s1 == s2 = (s1,Perc d1 d2)
    
    fg3 :: Double -> [String] -> [(String,Double)]
    fg3  r ss = zip ss (repeat r)

    fg4 :: [[String]]
    fg4 = map (map snd) lGroups




ls :: [(Double,String)]
ls = [ (0, "food")
     , (0, "other")
     , (1, "foo")
     , (3, "body")
     , (6, "panties")
     , (6, "nikki")
     , (98, "thingy")
     , (98, "jumpt")
     , (98, "jumper") ]



main = do
  let p :: (String,Perc) -> String
      p (s,Perc r1 r2) = printf "%15s %.3f %.3f" s r1 r2
  putStrLn $ unlines $ map p $ percentileTest ls
  
