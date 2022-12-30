
module Translation.ApplyTimeMod2 where

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Debug.Trace
import Text.Printf
import Control.Monad
import Control.Monad.State
import Control.Arrow
import Data.Map.Strict(Map)
import Data.Ratio
import Util.Math(scale)
import Util.Map(splitInclude)
import Common
import Common.CommonUtil
import Util.Exception
import Util.Math
import Score.ScoreData
import Translation

----------------------------------------------------------------------
----------------------------------------------------------------------


-- generalWarp
--
--   Change the flow of time between loc1 and loc2. (These are assumed
--   to be exact slice locations.)
--
--   The change in time can be expressed via the 'amt' parameter.
--
--     Left Double: a relative change in time *in seconds*
--
--     Right Double: a new absolute duration *in seconds*
--

generalWarp :: Map Int TimeSig -> RelTimeMap -> Loc -> Loc ->  
            Either Double Double -> UtmRampShape -> RelTimeMap
generalWarp ts tm loc1 loc2 amt shape =
         generalWarp2 ts tm loc1 loc2 amt shape

debugShowEither :: Either Double Double -> String
debugShowEither (Left x) = printf "(Left %.4f)" x
debugShowEither (Right x) = printf "(Right %.4f)" x


generalWarp2 :: Map Int TimeSig -> RelTimeMap -> Loc -> Loc ->  
            Either Double Double -> UtmRampShape -> RelTimeMap
generalWarp2 timeSigs (RelTimeMap tm) loc1 loc2 amt shape =
    RelTimeMap $ M.union slices3 tm
  where
    -- 'slices': list of [(Loc,Double)] for all slices within the affected
    -- range.
    slices = case filter (\(l,_) -> loc1 <= l && l < loc2) $ M.toList tm of
      xs@(_:_) -> xs
    -- 'dur1': original duration
    dur1 = sum $ map snd slices
    -- 'newDur': new duration
    newDur = case amt of
      Left delta | delta+dur1 > 0 -> delta+dur1
      Right x                     -> x
    simpleRatio = newDur/dur1
    deltaQuars :: Double
    deltaQuars = fromRational $ locDiffQuar timeSigs loc1 loc2
    -- Compute INITIAL MULTIPLY end ratios
    (rBeg,rEnd) = case shape of
      UrsTowardEnd  -> (1,simpleRatio)
      UrsFromBeg    -> (simpleRatio,1)
      UrsFlat       -> (1,1)
    -- 'g': process one slice  
    g (locIn,dIn) = (locIn,dIn*multiplier)
      where
       x = locDiffQuar timeSigs loc1 locIn
       multiplier = scale_3_2 0 (fromRational x) deltaQuars rBeg rEnd
    -- produce a list of slices in which slice duration is altered in a ramp
    -- pattern from 'rBeg' to 'rEnd'
    slices2 = map g slices
    -- now we must determine the duration of 'slices2'
    dur2 :: Double
    dur2 = sum $ map snd slices2
    -- now we must multiply every slice by a ratio such that the resulting sum
    -- of new slice durs will each targetDur
    slices3 = M.fromList $ map (\(l,d) -> (l,d*newDur/dur2)) slices2



-- applyTimeMod
--
applyTimeMod :: Map Int TimeSig -> RelTimeMap -> Utm -> RelTimeMap
                
-- applyTimeMod: UtmPause  
applyTimeMod timeSigs (RelTimeMap tm) (UtmPause _ locAt amtQuar) =
    RelTimeMap $ M.adjust (+amtSec) locPrev tm
  where
    amtSec = amtQuar * avgQuarDur timeSigs (RelTimeMap tm) loc1 loc2
    f d = locAddQuar timeSigs locAt d
    (loc1,loc2) = case (f (-1), f(1)) of
                    (Just l1,Just l2) -> (l1,l2)
    locPrev = case M.lookupLT locAt tm of
      Just (l,_) -> l


applyTimeMod timeSigs (RelTimeMap tm) (UtmPostPause _ locIn amtQuar) =
    RelTimeMap $ M.adjust (+amtSec) locIn tm
  where
    quarDur = lookupQuarDur timeSigs (RelTimeMap tm) locIn
    amtSec = quarDur * amtQuar


applyTimeMod timeSigs tm (UtmWarp _ loc1 loc2 shape eAmt) =
    generalWarp timeSigs tm loc1 loc2 (fmap (*qd) eAmt) shape
  where
    qd = avgQuarDur timeSigs tm loc1 loc2


applyTimeMod timeSigs (RelTimeMap tm) (UtmRamp _ loc1 loc2 tempo1 tempo2) =
    RelTimeMap $ M.mapWithKey g tm
  where
    s2 = locDiffQuar timeSigs loc1 loc2
    base = tempo2/tempo1
    g   loc d | loc < loc1  = d
              | loc >= loc2 = d
              | otherwise   = d/r 
      where
        s1  = locDiffQuar timeSigs loc1 loc
        r1 = scale 0 (fromRational s1) (fromRational s2) 0 1
        r = tempo1 * base ** r1
        

{- 
  RelTimeMap $
    
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

-}


----------------------------------------------------------------------
--                   --> Unit2Modify <--
{-

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
    
-}

{-
amtToSeconds :: TimeSigs -> RelTimeMap -> Loc -> Double -> Double
amtToSeconds timeSigs (RelTimeMap tm) loc1 amtQuarters =
  case M.lookup loc1 tm of
    Just d -> fromIntegral slicesPerBeat * multiplier * d
  where
    Loc msr
_ = loc1
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

avgQuarDur :: TimeSigs -> RelTimeMap -> Loc -> Loc -> Double
avgQuarDur timeSigs (RelTimeMap tm) loc1 loc2 = s / fromRational dr
  where
    s = sum . M.elems . snd . splitInclude loc1 . fst . M.split loc2 $ tm
    dr = locDiffQuar timeSigs loc1 loc2


makeOrdered :: Ord a => a -> a -> (a,a)
makeOrdered x y | x <= y     = (x,y)
                | otherwise  = (y,x)

                
--   <loc begin> <loc end>                
-- UnitWarpByPitch Loc Loc Int Double
   
-- UnitTimeMod

----------------------------------------------------------------------
{-

sliceFloor :: Loc -> Loc
sliceFloor (Loc msr beat) = Loc msr $ floor (beat*(spb%1)) % spb
  where
    spb = fromIntegral slicesPerBeat :: Integer
-}

----------------------------------------------------------------------
--                   find a delay that is expressed in quarters



{-

secPerQuarter :: TimeSigs -> RelTimeMap -> Loc -> Double
secPerQuarter ts (RelTimeMap tm) locIn =
  d*fromIntegral slicesPerBeat*fromIntegral denom/4
  where
    (Loc msr _,d) = case M.lookupLE locIn tm of {Just x -> x}
    TimeSig _ denom = case M.lookup msr ts of {Just x -> x}

-}
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
