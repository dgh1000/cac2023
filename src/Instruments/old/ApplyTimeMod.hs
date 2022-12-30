
module Instruments.ApplyTimeMod where

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M
import Debug.Trace
import Text.Printf
import Control.Monad
import Control.Monad.State
import Control.Arrow
import Data.Map(Map)
import Data.Ratio
import Util.Math(scale)
import Util.Map(splitInclude)
import Common
import Common.CommonUtil
import Util.Exception
import Util.Math
import Score.ScoreData
import Instruments
import Instruments.InstrumentsData


----------------------------------------------------------------------
----------------------------------------------------------------------


simpleWarp :: (Double -> Double) -> RelTimeMap -> Loc -> Loc -> RelTimeMap
simpleWarp ratioFn (RelTimeMap tm) loc1 loc2 =
  RelTimeMap $ M.mapWithKey g tm
  where
    -- compute total duration from loc1 to loc2. the amt change tells us the
    -- ratio to multiply each slice by
    total = sum . map snd .
            filter (\(l,_) -> loc1 <= l && l < loc2) $ M.toList tm
    ratio | r <= 0 = throwMine $ printf ("in applying warp between " ++
                     "locations %s and %s, a negative final duration"  ++
                     "resulted") (showLoc2 loc1) (showLoc2 loc2)
          | otherwise = r
      where r = ratioFn total
    g loc d | loc1 <= loc && loc < loc2 = ratio*d
            | otherwise = d


-- UnitRampWarp Loc Loc Double RampEndWhereOne
rampWarp :: Map Int TimeSig -> RelTimeMap -> Loc -> Loc ->  
            Double -> RampEndWhereOne -> RelTimeMap
rampWarp timeSigs (RelTimeMap tm) loc1 loc2 amt rampEnd =
  -- printf "loc1:%s loc2:%s amt:%.4f dur1:%.4f simpleRatio:%.4f"
  -- (showLoc2 loc1) (showLoc2 loc2) amt dur1 simpleRatio `trace`
    RelTimeMap result
  where
    slices = case filter (\(l,_) -> loc1 <= l && l < loc2) $ M.toList tm of
      xs@(_:_) -> xs
    dur1 = sum $ map snd slices
    simpleRatio = (dur1+amt)/dur1
    deltaQuars :: Double
    deltaQuars = fromRational $ locDiffQuar timeSigs loc1 loc2
    -- compute ratio for this slice
    (rBeg,rEnd) = case rampEnd of
      RewoBegin -> (simpleRatio,1)
      RewoEnd   -> (1,simpleRatio) 
      
    g (locIn,dIn) = (locIn,dIn*multiplier)
      where
       x = locDiffQuar timeSigs loc1 locIn
       multiplier = scale_3_2 0 (fromRational x) deltaQuars rBeg rEnd
    -- produce a list of slices in which slice duration is altered in a ramp
    -- pattern from 1 to ratio2
    slices2 = map g slices
    -- now we must determine the duration of 'slices2'
    dur2 :: Double
    dur2 = sum $ map snd slices2
    -- now we must multiply every slice by a ratio such that the resulting sum
    -- of new slice durs will each targetDur
    r = (dur1+amt)/dur2
    slices3 = map (\(l,d) -> (l,d*r)) slices2
    step :: Map Loc Double -> (Loc,Double) -> Map Loc Double
    step mIn (lIn,dIn) = M.insert lIn dIn mIn
    result = foldl step tm slices3



-- applyTimeMod
--
applyTimeMod :: Map Int TimeSig -> RelTimeMap -> UnitTimeMod ->
                RelTimeMap
    
--   This applies UnitWarp2, which has a list of Warp2Data.
--
--   This routine
--
--     determines average quarter duration across all locations covered by
--     Warp2Data.
--
--     this average duration A is used as follows: Warp2Data warp amounts W1,
--     W2, etc. are in quarters or beats, so this routine can compute the warp
--     amount in seconds by using W1*A, W2*A, etc.
--
--     handles each Warp2Data independently with a call to 'rampWarp'
--
applyTimeMod timeSigs (RelTimeMap tm) (UnitPostPause locIn amt) =
    RelTimeMap $ M.mapWithKey f tm
  where
    locFloor = sliceFloor locIn
    quarDur = lookupQuarDur timeSigs (RelTimeMap tm) locFloor
    f :: Loc -> Double -> Double
    f loc d | loc == locFloor = quarDur * fromRational amt + d
            | otherwise       = d

    
applyTimeMod timeSigs tm (UnitWarp2 datas) = foldl doOne tm datas
  where
    allLocsList = concatMap (\(Warp2Data loc1 loc2 _ _) -> [loc1,loc2]) datas
    allLocsLessMax = case S.maxView $ S.fromList allLocsList of
      Just (_,s) -> S.toList s
    len = length allLocsLessMax
    quarDurs = map (lookupQuarDur timeSigs tm) allLocsLessMax
    avgQuarDur = sum quarDurs / fromIntegral len
    quarDurToUse = case quarDurs of
      d:ds | all (==d) ds -> d
           | otherwise    -> throwMine ("in applying a UnitWarp2, quarter " ++
                             "note durations at all warp locs are not " ++
                             "the same")
    doOne :: RelTimeMap -> Warp2Data -> RelTimeMap
    doOne tmIn (Warp2Data loc1 loc2 amt re) =
      rampWarp timeSigs tmIn loc1 loc2 amtd re
      where
        amtd = avgQuarDur * amt


applyTimeMod timeSigs (RelTimeMap tm) (UnitAbsWarp loc1 loc2 amt) =
  simpleWarp (\t -> amtd/t) (RelTimeMap tm) loc1 loc2
   where
     amtd = fromRational amt * lookupQuarDur timeSigs (RelTimeMap tm) loc1


applyTimeMod timeSigs (RelTimeMap tm) (UnitPause loc2 amt) =
  simpleWarp (\t -> (t+amtd)/t) (RelTimeMap tm) loc1 loc2
  where
    amtd = fromRational amt * lookupQuarDur timeSigs (RelTimeMap tm) loc2
    loc1 = case M.lookupLT loc2 tm of
      Nothing -> throwMine $ printf ("pause at %s seems to be too close" ++
                 " to beginning of time map") (showLoc2 loc1)
      Just (k,_) -> k


----------------------------------------------------------------------
--                   --> Unit2Modify <--
applyTimeMod timeSigs (RelTimeMap tm)
  (Unit2Modify loc1 loc2 loc3 tm1 tm2 mNormalize) = case mNormalize of
    Just norm -> let origSum = sum1+sum2
                     newSum = sumM1+sumM2
                     targetSum = (origSum-newSum)*norm + newSum
                     ratio | newSum > 0 = targetSum/newSum
                 in RelTimeMap $ tmInsertRange tm
                               $ map (second (*ratio)) together
    Nothing -> RelTimeMap $ tmInsertRange tm together 
  where
    l1 = sliceFloor loc1
    l2 = sliceFloor loc2
    l3 = sliceFloor loc3
    (n1,sum1,range1) = tmSplitOutRange l1 l2 tm
    (n2,sum2,range2) = tmSplitOutRange l2 l3 tm
    (sumM1,range1M) = applyTempoModify n1 range1 tm1 
    (sumM2,range2M) = applyTempoModify n2 range2 tm2
    together = range1M ++ range2M
    


{-
amtToSeconds :: TimeSigs -> RelTimeMap -> Loc -> Double -> Double
amtToSeconds timeSigs (RelTimeMap tm) loc1 amtQuarters =
  case M.lookup loc1 tm of
    Just d -> fromIntegral slicesPerBeat * multiplier * d
  where
    Loc msr _ = loc1
    multiplier = case M.lookup msr timeSigs of
      Just (TimeSig n d) -> fromIntegral d / 4
-}


lookupQuarDur :: TimeSigs -> RelTimeMap -> Loc -> Double
lookupQuarDur timeSigs (RelTimeMap tm) loc1 =
  case M.lookup loc1 tm of
    Just d -> fromIntegral slicesPerBeat * multiplier * d
  where
    Loc msr _ = loc1
    multiplier = case M.lookup msr timeSigs of
      Just (TimeSig n d) -> fromIntegral d / 4


makeOrdered :: Ord a => a -> a -> (a,a)
makeOrdered x y | x <= y     = (x,y)
                | otherwise  = (y,x)
    


----------------------------------------------------------------------

sliceFloor :: Loc -> Loc
sliceFloor (Loc msr beat) = Loc msr $ floor (beat*(spb%1)) % spb
  where
    spb = fromIntegral slicesPerBeat :: Integer


----------------------------------------------------------------------
--                   find a delay that is expressed in quarters



secPerQuarter :: TimeSigs -> RelTimeMap -> Loc -> Double
secPerQuarter ts (RelTimeMap tm) locIn =
  d*fromIntegral slicesPerBeat*fromIntegral denom/4
  where
    (Loc msr _,d) = case M.lookupLE locIn tm of {Just x -> x}
    TimeSig _ denom = case M.lookup msr ts of {Just x -> x}


----------------------------------------------------------------------
----------------------------------------------------------------------
--                     useful utilities


    
findPauseLoc :: Map Loc Double -> Loc -> Loc
findPauseLoc m loc = case M.toDescList . fst . M.split locFloor $ m of
    (locP,_):_ -> locP
  where
    locFloor = case M.lookupLE loc m of {Just (x,_) -> x}


-- tmSplitOutRange

tmSplitOutRange :: Loc -> Loc -> Map Loc Double ->
                   (Int,Double,[(Int,(Loc,Double))])
tmSplitOutRange loc1 loc2 tm =
    (length items, sum $ map snd items, zip [1..] items)
  where
    items = M.toAscList $ snd $ splitInclude loc1 $ fst $ M.split loc2 tm
    


tmInsertRange :: Map Loc Double -> [(Loc,Double)] -> Map Loc Double
tmInsertRange = foldr (\(l,d) m -> M.insert l d m)


applyTempoModify :: Int -> [(Int,(Loc,Double))] -> TempoModify ->
                    (Double,[(Loc,Double)])
applyTempoModify n items tmod = (sum $ map snd segs, segs)
  where
    segs = map (\(i,(l,d)) -> (l,d/f i)) items
    f idx = case tmod of
      TmRamp r1 r2 -> scale 1 (fromIntegral idx) (fromIntegral n) r1 r2
      TmRampParab r1 r2 False ->
        scale 1 (fromIntegral idx**2) (fromIntegral n**2) r1 r2
      TmRampParab r1 r2 True ->
        scale (fromIntegral n**2) (fromIntegral (n+1-idx)**2) 1 r1 r2
