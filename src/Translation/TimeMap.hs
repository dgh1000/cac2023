
module Translation.TimeMap where

import qualified Data.Map.Strict as M
import qualified Data.List as L
import qualified Data.Set as S
import System.IO.Unsafe
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
import Translation
import Translation.ApplyTimeMod2
import Translation.ToUnitTimeMods2
import Score.ScoreData
    ( Mark(RampEndBeg, SetTempo, RitAccel, RampBeg, RampEnd),
      MarkD,
      Score(scStaves, scTimeSigs, scMarks),
      Staff(stMaxTrueEnd) )
import Util.Exception
import Util.Math
import Util.Map
import Util.RandomState
import Common.CommonUtil
import Common


-- we have time map go out to max true end of all staves plus one measure
--
-- so 
--
-- 

computeBaseTimeMap :: Score -> Double -> RelTimeMap 
computeBaseTimeMap score ratio = RelTimeMap tm2
  where
    mte' = maxTrueEndAllStaves score
    mte = Loc (msrNum mte' + 1) (beat mte')
    timeSigs = scTimeSigs score
    -- parse level 1 tempos
    -- The MarkD constructors this responds to are SetTempo and RitAccel.
    t1s = filterLev1Tempos timeSigs $ scMarks score
    tm1 = doSegmentsLev1 timeSigs t1s mte
    -- add level 2 tempos
    -- The MarkD constructors this responds to are RampBeg, RampEnd, and RampEndBeg
    t2s = filterLev2Tempos timeSigs $ scMarks score
    tm2 = doLevel2 timeSigs t2s tm1


dumpTimeMap :: TimeSigs -> Map Loc Double -> IO (Map Loc Double)
dumpTimeMap timeSigs tm = do
  let g :: (Loc,Double) -> String
      g (Loc msr beat,d) = printf "%.4f, %4f\n" t d
        where
          t :: Double
          t = fromIntegral msr + fracPart
          TimeSig numer _ = timLookup "a" msr timeSigs
          fracPart = (fromRational beat-1)/fromIntegral numer
  writeFile "out.csv" $ concatMap g $ M.toAscList tm
  return tm


filterLev1Tempos :: Map Int TimeSig -> Map Loc (Map String [MarkD]) ->
                    Map Loc TempoLev1
filterLev1Tempos timeSigs m = out2
  where
    out = M.mapMaybe maybeTempo1 (M.map (concat . M.elems) m)
    out2 = M.mapKeysWith (error "enb81") (roundToSlice timeSigs) out


maybeTempo1 :: [MarkD] -> Maybe TempoLev1
maybeTempo1 ms = case mapMaybe g ms of
  [] -> Nothing
  [t] -> Just t
  where
    -- T1SetTempo <maybe pre-tempo> <current or post-tempo> <flag: * present>
    g :: MarkD -> Maybe TempoLev1
    g (SetTempo md d flag) = Just $ T1SetTempo md d flag
    g RitAccel             = Just T1RitAccel
    g _                    = Nothing


maxTrueEndAllStaves :: Score -> Loc
maxTrueEndAllStaves = maximum . map stMaxTrueEnd . M.elems . scStaves
    

doSegmentsLev1 :: TimeSigs -> Map Loc TempoLev1 -> Loc -> Map Loc Double
doSegmentsLev1 tsIn mapIn endLoc = M.fromList $ concat segs
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


-- Given
--          Loc of a slice range
--          Level 1 tempo map
--
-- compute
--
--          all slices within that range
--
doSegment :: TimeSigs -> Map Loc TempoLev1 -> Loc -> [(Loc,Double)]
doSegment ts mapIn loc = case M.lookup loc mapIn of
  Just T1RitAccel ->
    oneRangeSlices ts $ doRitAccel 
  Just (T1SetTempo _ tLeft flag) ->
    oneRangeSlices ts $ doSetTempo tLeft flag
  where
    -- w:
    --
    -- compute
    --           right tempo of segment beginning at locR
    --
    --                 locR is loc of segment in mapIn just to the
    --                 right of input 'loc'
    --
    w tLeft isRitAccel = case M.lookupGT loc mapIn of
      Just (locR,T1SetTempo (Just x) t2 _)
        | isRitAccel -> (locR,t2)
        | otherwise  -> (locR,x)
      Just (locR,T1SetTempo _        t2 _)
        | not isRitAccel -> (locR,tLeft)
        | otherwise      -> "should be" `trace` (locR,t2)
      Just (locR,T1RitAccel) -> (locR,tLeft)
     --
    -- doRitAccel
    --
    --   given
    --           Loc of an individual slice
    --   compute
    --           The argument
    --                  ((Loc,Double),(Loc,Double))
    --           to oneRangeSlices
    --
    doRitAccel = ((loc,tLeft),w tLeft True)  
      where
        tLeft = case M.lookupLT loc mapIn of
          Just (_,T1SetTempo _ tempo False) -> tempo
          Just (_,T1SetTempo _ _     True ) ->
            throwMine $ printf ("level 1 tempo segment, starting at %s " ++
            "with a RitAccel: tempo mark before RitAccel is a ramp type")
            (showLoc2 loc)
    -- the case that mark at 'loc' is a SetTempo of tempo 'tLeft'
    -- 
    doSetTempo tLeft flag = ((loc,tLeft),w tLeft False)

-- oneRangeSlices
--
--    Compute
--
--             slices [(Loc,Double)]
--
--    spanning one 'interval'
--
--    in Level 1 tempo map
--
oneRangeSlices :: TimeSigs -> ((Loc,Double),(Loc,Double)) -> [(Loc,Double)]
oneRangeSlices timeSigs
  ((locL,tempoL),(locR,tempoR)) = filter pred rawSlices
  --  (tL@(lL@(Loc msrL _),tempoL),tR@(lR@(Loc msrR _),tempoR)) =
  where
    msrs | msrNum locL <= msrNum locR = [msrNum locL..msrNum locR]
    rawSlices =
      {- printf "locL: %s tempoL:%.4f locR:%s tempoR:%.4f" (showLoc2 locL)
        tempoL (showLoc2 locR) tempoR `trace` -}
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
    startDur = 60/(tempoL * (fromIntegral denom*24/4))
    base = tempoL/tempoR
    (exponentMsrL,exponentMsrR) = computeExponents timeSigs msrNum t1 t2
    TimeSig numer denom = timLookup "b" msrNum timeSigs
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





-- First round beats by round (n*x) / n. Then check if we are at beat 1 of
-- next measure.
roundToSlice :: Map Int TimeSig -> Loc -> Loc
roundToSlice timeSigs (Loc msr beat) = Loc newMsr newBeat
  where
    numer = fromIntegral . tsNumer . fromJust . M.lookup msr $ timeSigs
    n :: Rational
    n = fromIntegral slicesPerBeat
    b :: Rational
    b = (fromIntegral $ round $ n*beat) / n
    (newBeat,newMsr) | b <  numer+1 = (b, msr  )
                     | b == numer+1 = (1, msr+1)

----------------------------------------------------------------------
--                RANDOMIZE

randomizeTimeMap :: RelTimeMap -> Tr RelTimeMap
randomizeTimeMap (RelTimeMap tm) = do
  let f :: (Loc,Double) -> Tr (Loc,Double)
      f (loc,d) = do
        ratio <- rdRandomR 0.85 1.15
        return (loc, ratio*d)  -- CHANGE BACK TO D*RATIO
  results <- mapM f (M.toAscList tm)
  return $ RelTimeMap $ M.fromList results
  --  (RelTimeMap . M.fromList) `liftM` (mapM f $ M.toAscList tm)


----------------------------------------------------------------------
----------------------------------------------------------------------
--                 Level 2 (TWO)


filterLev2Tempos :: TimeSigs -> Map Loc (Map String [MarkD]) ->
                    Map Loc TempoLev2
filterLev2Tempos timeSigs m = out2
  where
    out = M.mapMaybe maybeTempo2 (M.map (concat . M.elems) m)
    out2 = M.mapKeysWith (error "m3bv8s") (roundToSlice timeSigs) out


maybeTempo2 :: [MarkD] -> Maybe TempoLev2
maybeTempo2 ms = case mapMaybe g ms of
  []  -> Nothing
  [x] -> Just x
  where
    g :: MarkD -> Maybe TempoLev2
    g (RampBeg r1)       = Just $ T2Ramp Nothing (Just r1)
    g (RampEnd r1)       = Just $ T2Ramp (Just r1) Nothing
    g (RampEndBeg r1 r2) = Just $ T2Ramp (Just r1) (Just r2)
    g _                  = Nothing


data OneRamp = OneRamp Loc Double Loc Double

doLevel2 :: TimeSigs -> Map Loc TempoLev2 -> Map Loc Double -> Map Loc Double
doLevel2 timeSigs marks tmIn = foldl (doOneRamp timeSigs) tmIn ramps
  where
    ramps = extractRamps marks
    


extractRamps :: Map Loc TempoLev2 -> [OneRamp]
extractRamps marks = mapMaybe computeOneRamp keys
  where
    keys = M.keys marks
    computeOneRamp :: Loc -> Maybe OneRamp
    computeOneRamp loc = case M.lookup loc marks of
      Just (T2Ramp _ (Just r1)) -> case M.lookupGT loc marks of
        Just (locR,T2Ramp (Just r2) _) -> Just $ OneRamp loc r1 locR r2
        _ -> throwMine $ printf ("in extractRamps, a left level 2 ramp " ++
             "at %s was followed by a level 2 something that's not a right "++
             "ramp") (showLoc2 loc)
      _ -> Nothing


-- doOneRamp
--
--
--   r1 = 1
--   r2 = 0.5
--
--   r1 * b ** (scale loc1 l loc2 0 1)
--
--   b = r2/r1
--
doOneRamp :: TimeSigs -> Map Loc Double -> OneRamp -> Map Loc Double
doOneRamp timeSigs tmIn (OneRamp loc1 r1 loc2 r2) = M.mapWithKey g tmIn
  where
    rr1 = 1/r1
    rr2 = 1/r2
    base = rr2/rr1
    diff2 = locDiffQuar timeSigs loc1 loc2
    g :: Loc -> Double -> Double
    g locMid d
      | locMid < loc1 = d
      | locMid >= loc2 = d
      | otherwise = d * rr1 * (base ** amt)
      where
        amt = scale 0 (fromRational diff1) (fromRational diff2) 0 1
        diff1 = locDiffQuar timeSigs loc1 locMid


----------------------------------------------------------------------
--                     time lookup


lookupTime :: Loc -> AbsTimeMap -> Double
lookupTime loc (AbsTimeMap tm) = case M.lookup loc tm of
  Just t -> t
  Nothing -> 
    let (loc1,t1) = case M.lookupLT loc tm of {Just x  -> x}
        (   _,t2) = case M.lookupGT loc tm of
                      Just x  -> x
                      Nothing -> throwMine $ printf
                                 ("in lookupTime, no time found for " ++
                                 "loc %s: perhaps a dynamic is " ++
                                 "placed past the last note?") (showLoc2 loc)
        Loc _ b  = loc
        Loc _ b1 = loc1
        r = fromRational $ (b-b1)/(1%fromIntegral slicesPerBeat)
    in t1 + (t2-t1)*r


-- Given a Duration, compute it in seconds
-- 
-- DurSecs is easy.
--
-- DurBeats means we need to find the duration of a beat at the given
-- Loc. Look up relevant slice and take its duration.
computeDurFromBeats :: Loc -> RelTimeMap -> Double -> Double
computeDurFromBeats loc (RelTimeMap tm) nBeats = case M.lookupLE loc tm of
  Just (_,sliceD) -> sliceD*nBeats*fromIntegral slicesPerBeat




----------------------------------------------------------------------
----------------------------------------------------------------------
--                     conversion to absolute

toAbsolute :: RelTimeMap -> AbsTimeMap
toAbsolute (RelTimeMap m) = AbsTimeMap . snd . M.mapAccum g 0 $ m
  where
    g :: Double -> Double -> (Double,Double)
    g timeIn sliceDur = (timeIn+sliceDur,timeIn)
    

----------------------------------------------------------------------

-- timeToLoc
--
-- Convert a time in seconds to a Loc, **to facilitate a debug dump of
-- of a dynamics curve**
--
-- 
--
-- the error here occurs under following condition
--
--   an input time d is given. we 
timeToLoc :: Double -> TimeSigs -> Map Double Loc -> Loc
timeToLoc d timeSigs m = case locAddQuar timeSigs l1 dQuar of
  Just x -> x
  Nothing -> error $ printf "l1: %s dq:%s %d" (showLoc2 l1) (show l2) maxMsr
  where
    maxMsr = case M.maxViewWithKey timeSigs of {Just ((i,_),_) -> i}
    (d1,l1) = case M.lookupLE d m of {Just a -> a}
    (d2,l2) = case M.lookupGT d m of {Just a -> a}
    qs = fromRational $ locDiffQuar timeSigs l1 l2
    dQuar = approxRational (scale d1 d d2 0 qs) 0.125


locsRoundToSlice :: Map Int TimeSig -> Map Loc (Map String [MarkD]) ->
                    Map Loc (Map String [MarkD])
locsRoundToSlice timeSigs =
  M.mapKeysWith (M.unionWith (++)) (roundToSlice timeSigs)


timLookup msg k m = case M.lookup k m of {Just x -> x; Nothing -> throwMine msg}
