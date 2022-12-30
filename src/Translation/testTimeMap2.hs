import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Set as S
import Text.Printf
import Control.Monad.State
import Control.Arrow
import Data.Either
import Data.Maybe
import Data.Map.Strict(Map)
import Data.List(tails)
import Data.Ratio
import Data.Set(Set)
import Debug.Trace
import Text.Printf
import Instruments
import Instruments.InstrumentsData
import Instruments.ApplyTimeMod2
import Instruments.ToUnitTimeMods2
import Score.ScoreData
import Util.Exception
import Util.Math
import Util.Map
import Util.RandomState
import Common.CommonUtil
import Common

tsData = M.fromList [ (1, TimeSig 4 4)
                    , (2, TimeSig 2 2)
                    , (3, TimeSig 4 4)
                    , (4, TimeSig 4 4)
                    , (5, TimeSig 4 4) ]

marks :: Map Loc TempoLev1
marks = M.fromList [ (Loc 1 2, T1SetTempo Nothing 60 True)
                   , (Loc 1 3, T1RitAccel)
                   , (Loc 4 1, T1SetTempo (Just 30) 30 False)
                   ]


main = do
  let slices = doSegments tsData marks (Loc 5 1) 
      g :: Rational -> Int
      g beat = round $ fromIntegral slicesPerBeat * (beat - 1)
      g2 :: Loc -> Int
      g2 loc = round $
               fromIntegral slicesPerBeat * locDiffQuar tsData (Loc 1 1) loc
      g3 :: Loc -> Double
      g3 (Loc msr beat) = fromIntegral msr + fracPart
        where
          TimeSig numer _ = tmLookup msr tsData
          fracPart = (fromRational beat-1)/fromIntegral numer
      f :: (Loc,Double) -> String
      f (loc,dur) = printf "%.4f, %.4f\n" (g3 loc) dur  
  writeFile "out.csv" $ concatMap f slices 


doSegments :: TimeSigs -> Map Loc TempoLev1 -> Loc -> [(Loc,Double)]
doSegments tsIn mapIn endLoc = concat segs
  where
    ts = addTimeSigFinalMeasure tsIn
    addTrueEndLoc m = case M.lookupGE endLoc m of
      Nothing -> M.insert endLoc (T1SetTempo Nothing 60 False) m
    addFirstLoc m = case M.lookup (Loc 1 1) m of
      Nothing -> M.insert (Loc 1 1) (T1SetTempo Nothing 60 False) m
      Just _  -> m
    segs :: [[(Loc,Double)]]
    m2 = addFirstLoc mapIn
    keysToIterateOn = M.keys m2
    m3 = addTrueEndLoc m2
    -- Note: iterating over keys in **mapIn** but using **map2** (which has
    -- the extra key added at 'endLoc'
    segs = map (doSegment ts m3) keysToIterateOn


doSegment :: TimeSigs -> Map Loc TempoLev1 -> Loc -> [(Loc,Double)]
doSegment ts mapIn loc = case M.lookup loc mapIn of
  Just T1RitAccel -> oneRangeSlices ts $ doRitAccel 
  Just (T1SetTempo _ tLeft flag) -> oneRangeSlices ts $ doSetTempo tLeft flag
  where
    -- figure right tempo of segment
    w tLeft = case M.lookupGT loc mapIn of
      Just (locR,T1SetTempo (Just x) _ _) -> (locR,x)
      Just (locR,_                      ) -> (locR,tLeft)
    doRitAccel = ((loc,tLeft),w tLeft)  
      where
        tLeft = case M.lookupLT loc mapIn of
          Just (_,T1SetTempo _ tempo False) -> tempo
          Just (_,T1SetTempo _ _     True ) ->
            throwMine $ printf ("level 1 tempo segment, starting at %s " ++
            "with a RitAccel: tempo mark before RitAccel is a ramp type")
            (showLoc2 loc)
    -- the case that mark at 'loc' is a SetTempo of tempo 'tLeft'
    doSetTempo tLeft flag = printf "Set Tempo tLeft: %.4f" tLeft
       `trace` ((loc,tLeft),w tLeft)

-- what should happen is that it constructs a segment

oneRangeSlices :: TimeSigs -> ((Loc,Double),(Loc,Double)) -> [(Loc,Double)]
oneRangeSlices timeSigs
  ((locL,tempoL),(locR,tempoR)) = filter pred rawSlices
  --  (tL@(lL@(Loc msrL _),tempoL),tR@(lR@(Loc msrR _),tempoR)) =
  where
    msrs | msrNum locL <= msrNum locR = [msrNum locL..msrNum locR]
    rawSlices =
      printf "locL: %s tempoL:%.4f locR:%s tempoR:%.4f" (showLoc2 locL)
        tempoL (showLoc2 locR) tempoR `trace`
        concatMap (rawMsrSlices timeSigs (locL,tempoL) (locR,tempoR)) msrs
    pred (l,_) = locL <= l && l < locR


-- rawMsrSlices
--
--   Given a segment, which ranges from locL to locR (this range may span more
--   than one measure), and given a measure number N, where N is within the
--   range (msrNum locL) to (msrNum locR), construct all slices that are part
--   of measure N.
--
--   
rawMsrSlices :: TimeSigs -> (Loc,Double) -> (Loc,Double) -> Int ->
               [(Loc,Double)]
rawMsrSlices timeSigs t1@(locL,tempoL) t2@(locR,tempoR) msrNum =
    map f [0..numSlices-1]
  where
    startDur = 60/tempoL
    base = tempoL/tempoR
    (exponentMsrL,exponentMsrR) = computeExponents timeSigs msrNum t1 t2
    TimeSig numer denom = tmLookup msrNum timeSigs
    numQuars :: Double
    numQuars = 4*fromIntegral numer/fromIntegral denom
    numSlices = numer * slicesPerBeat
    f :: Int -> (Loc,Double)
    f sliceN = (Loc msrNum beatN, startDur*(base**e))
      where
        beatN :: Rational
        beatN = 1 + fromIntegral sliceN % fromIntegral slicesPerBeat
        e = scale_3_2 0 (fromIntegral sliceN) (fromIntegral numSlices)
                        exponentMsrL exponentMsrR


-- exponent ranges from one 
computeExponents :: TimeSigs -> Int -> (Loc,Double) -> (Loc,Double) ->
                    (Double,Double)
computeExponents timeSigsIn msrN (locL,tempo1) (locR,tempo2) = (frac1,frac2)
  where
    timeSigs = addTimeSigFinalMeasure timeSigsIn
    -- we are finding 
    locDiffSigned loc1 loc2 = lds loc1 loc2
    lds locIn1 locIn2
      | locIn1 <  locIn2  =   locDiffQuar timeSigs locIn1 locIn2
      | locIn1 == locIn2  =   0
      | locIn1 >  locIn2  = - locDiffQuar timeSigs locIn2 locIn1
    mLoc1 = Loc msrN     1
    -- why do we need to compute this part? we need to use it in locDiff
    -- how about add a final msr to timeSigs maybe?
    mLoc2 = Loc (msrN+1) 1
    d1 = locDiffSigned locL mLoc1
    d2 = locDiffSigned locL mLoc2
    d3 = locDiffSigned locL locR
    frac1 :: Double
    frac1 = scale_3_2 0 (fromRational d1) (fromRational d3) 0 1
    frac2 :: Double
    frac2 = scale_3_2 0 (fromRational d2) (fromRational d3) 0 1


addTimeSigFinalMeasure :: TimeSigs -> TimeSigs
addTimeSigFinalMeasure ts = case M.maxViewWithKey ts of
  Just ((maxMsrNum,timeSig),_) -> M.insert (maxMsrNum+1) timeSig ts 


tmLookup k m = case M.lookup k m of {Just x -> x}

