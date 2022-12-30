
module Translation.Trill where


import qualified Data.Map.Strict as M
import Text.Printf
import Data.Map.Strict(Map)
import Data.Maybe 
import Data.List (unfoldr, find, mapAccumL)
import Util.Math (scale)
import Score.ScoreData
import Common
import Common.CommonUtil
import Util.Map
import Util.Exception
import Translation

----------------------------------------------------------------------
----------------------------------------------------------------------
--             data needed in this module

data Fold = Fold 
  { fTime :: Double
  , fStep :: TrillStep
  }

----------------------------------------------------------------------
----------------------------------------------------------------------
--                main function: trill times


-- trillTimes
--
--   output (<trill1> , <trill2> )
--
--   Where
--
--     trill1 is trill time data for:
--
--       tremolo: first chord occurring in score
--
--       trill: upper note
--
--     trill2 is trill time data for:
--
--       tremolo 2nd note, or trill lower note
--


trillTimes :: TrillShape -> Double -> Double -> ([TrillData],[TrillData])
trillTimes tShape@(TrillShape step1 _ _) tMin tMax =
  (map toTrillData outLower,map toTrillData outUpper)

  where
    step2 | step1 == Upper = Lower
          | step1 == Lower = Upper
    nsSteps = zip (cycle [step1,step2]) [0..]
    -- compute 'ts1': a rough estimate of trill times (they don't necessarily
    --      end right on tMax)
    ts1 :: [(Double,Double)]
    ts1 = trillTimes2 tShape (tMax-tMin)
    nSteps = length ts1
    -- compute a multiplier 'ratio' to adjust actual trill times to
    -- exactly fit in tMin to tMax
    roughDur | not (null ts1) = let (_,t) = last ts1 in t
    ratio = (tMax-tMin)/roughDur
    -- change time to absolute, while multiplying by 'ratio'
    outPairs = map (\(a,b) -> (tMin+a*ratio,tMin+b*ratio)) ts1
    -- pair times with steps
    out :: [((TrillStep,Int),(Double,Double))]
    out = zip nsSteps outPairs
    -- now filter to preserve only lower
    takeStep ((s,_),_) = s
    outLower = filter ((==Lower) . takeStep) out
    outUpper = filter ((==Upper) . takeStep) out
    toTrillData ((_,i),(t1,t2)) = TrillData i nSteps t1 t2


{-
trillTimes :: TrillShape -> Double -> Double ->
              [((TrillStep,Int),(Double,Double))]
trillTimes tShape@(TrillShape step1 _ _) tMin tMax = zip nsSteps outPairs
  where
    nsSteps = zip (cycle [step1,step2]) [0..]
    outPairs = map (\(a,b) -> (tMin+a*ratio,tMin+b*ratio)) ts1
    step2 | step1 == Upper = Lower
          | step1 == Lower = Upper
    ts1 = trillTimes2 tShape (tMax-tMin)
    roughDur | not (null ts1) = let (_,t) = last ts1 in t
    ratio = (tMax-tMin)/roughDur
-}    

-- trillTimes2
--
-- Compute first approximation of trill times. Final time may not be equal to
-- dur.
trillTimes2 :: TrillShape -> Double -> [(Double,Double)]
trillTimes2 (TrillShape step1 segs step2) dur = unfoldr step (0,step1)
  where
    tab = makeTable segs dur
    step :: (Double,TrillStep) -> Maybe ((Double,Double),(Double,TrillStep))
    step (t1,nextStep)
      | t1 > dur-d/2 && nextStep /= step2 = Nothing
      | otherwise = Just ( (t1,t2)
                         , (t2, if nextStep==Upper then Lower else Upper) )
      where
        d = 1/tableLookup tab t1
        t2 = t1+d
        
{-        
    step :: Fold -> Maybe ((Double,Double,TrillStep),Fold)
    step (Fold t1 nextStep) 
        | t1 > dur-d/2 && nextStep /= step2 = Nothing
        | otherwise = Just ((t1,t2,nextStep),f)
        where
          d = 1/tableLookup tab t1
          t2 = t1 + d
          f = Fold t2 (if nextStep == Upper then Lower else Upper)
-}


-- makeTable
--   Make pairs (<trill rate>, <t start>)
makeTable :: [(Double,Int)] -> Double -> [(Double,Double)]
makeTable segs dur
  | not (null segs) = zip times rates
  where
    rates = let r = map fst segs in r ++ [last r]
    durUnits = dur / (sum $ map (fromIntegral . snd) segs)
    times = scanl (+) 0 $ map (\(_,d) -> durUnits * fromIntegral d) segs


{-
makeTable :: Double -> Double -> [(Int,Int)] -> Double -> [(Double,Double)]
makeTable rateMin rateMax segs dur
  | not (null segs) = zip ts rates
  where
    rates = let r = map (\(i,_) -> 
                         scale 0 (fromIntegral i) 9 rateMin rateMax) segs
            in r ++ [last r]
    durUnits = dur / (sum $ map (fromIntegral . snd) segs)
    ts = scanl (+) 0 $ map (\(_,d) -> durUnits * fromIntegral d) segs
-}

-- tableLookup
--
-- [(Double,Double)] :: [(<time of beginning of segment>,<value at beginning>)]
tableLookup :: [(Double,Double)] -> Double -> Double
tableLookup segs t = case segs of
  [(_,i)] -> i
  ss -> let lst | not (null ss) = last ss
            minT = fst $ head ss
            maxT = fst $ lst
            minV = snd $ head ss
            maxV = snd $ lst
            paired = zip ss (tail ss)
        in case (t < minT, t > maxT) of
             (True ,     _) -> minV
             (_    , True ) -> maxV
             (False, False) -> case find matchingSeg paired of
                 Just ((t1,v1),(t2,v2)) -> scale t1 t t2 v1 v2
  where
    matchingSeg ((t1,_),(t2,_)) = t1 <= t && t <= t2

----------------------------------------------------------------------
----------------------------------------------------------------------
--                  extract trill map

{-

data MFold = MFold (Double,Double)


extractTrill :: Map Loc [Mark] -> Map Loc (Double,Double,TrillShape)
extractTrill = M.alter f (Loc 1 1) . extractTrill2
  where
    f Nothing  = Just (4,15,TrillShape Upper [(8,1)] Lower)
    f (Just x) = Just x



extractTrill2 :: Map Loc [Mark] -> 


extractTrill2 :: Map Loc [Mark] -> Map Loc (Double,Double,TrillShape)
extractTrill2 = M.fromListWithKey err . catMaybes . snd . 
                mapAccumL step (4,15) . lMapToList
  where
    err k _ _ = throwMine "can't have two trill shapes at %s" 
                (simpleShowLoc k)
    maybeTrillShape (TrillShapeMark s) = Just s
    maybeTrillShape _                  = Nothing
    maybeTrillRates (TrillRates d1 d2) = Just (d1,d2)
    maybeTrillRates _                  = Nothing
    step :: (Double,Double) -> (Loc,Mark) -> 
            ((Double,Double), Maybe (Loc,(Double,Double,TrillShape)))
    step (r1,r2) (loc,m) = case (maybeTrillShape m, maybeTrillRates m) of
      (Just s, _             ) -> ((r1 ,r2 ), Just (loc,(r1,r2,s)))
      (_     , Just (r11,r22)) -> ((r11,r22), Nothing             )
      _                        -> ((r1, r2 ), Nothing             )
-}
