{-# LANGUAGE TupleSections #-}
module Instruments.Dynamics where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Common as CD
import System.IO.Unsafe
import Control.Monad.State
import Control.Arrow
import Debug.Trace
import Text.Printf
import Data.Maybe
import Data.Map.Strict(Map)
import Data.Ratio
import Data.Monoid
import Common
import Common.CommonUtil
import Score.ScoreExport
import Score.ScoreData
import Instruments.TimeMap
import Instruments
import Instruments.Curves
import Instruments.InstrumentsData
import Instruments.ShowInstruments
import Util.Exception
import Util.Math(scale)
import Util.Showable
import Util.Map


-- we need to add little segments boosting dynamics or dropping dynamics. 

------------------------------------------------------------------------
--            NEW NEW NEW 2018 2018
--
-- incorporating more sophisticated dynamic (below-staff) markings
--
-- level 1 and level 2 concept
--
--   with tempo, my ramps are essentially level 2
--
-- primary dynamics
--
--   
--
-- level 1 dynamics
--
--   p, f, etc. and result of applying ramps
--
--
-- hairpins:
--
--   are used to compute level 1 dynamics
--
--   will only be allowed now when the prior dynamic mark is NOT a ramp type
--
-- level 2 dynamics
--
--   mark a region in which dynamics are alterated from main dynamics
--
--   well we could use delta marks as.
--
--   we could use = signs, just like tempo changes
--
-- single point level 2 dynamics
--
-- we now have different ways of expressing dynamics
--
-- what does it mean to have delta mark in the middle of a ramp? ramp always
-- goes to next delta mark
--
-- terrmination
--
--   ramped dynamic mark: one ending in *, indicating it ramps to next mark
--
--       pp*  f*
--
--   dynamic pre-mark
--
--      p|f
--
--
-- algorithm:
--
--   build level 1 without ramps
--
--   add ramps via 
--

----------------------------------------------------------------------
----------------------------------------------------------------------


-- handle hairpins:
--
--   1. fold them into map of level 1 dynamic marks. problem is that there
--      might be a hairpin start at same loc as dynamic mark 


hpDynCurves :: Map String AbsTimeMap -> Score -> Map String Curve
hpDynCurves atms score = M.map f $ scStaves score
  -- unsafePerformIO (dumpCurves $ M.map f $ scStaves score)
  where
    f :: Staff -> Curve
    f staff = Curve [lev1OneCurve, lev2OneCurve]
      where
        staffMarks = dLookup (stName staff) (scMarksByStaff score)
        atm = dLookup (stName staff) atms
        dynMarks = lMapMaybe maybeDynMark staffMarks
        lev1OneCurve = staffLev1 (scTimeSigs score) atm dynMarks staff
        lev2OneCurve = staffLev2 (scTimeSigs score) atm dynMarks staff


data DynMark = DmLev1Simple Double
             | DmLev1R Double
             | DmLev1L Double
             | DmLev2R Double
             | DmLev2L Double
              deriving(Eq,Ord)


maybeDynMark :: MarkD -> Maybe DynMark
maybeDynMark (Lev1DynL v) = Just $ DmLev1L v
maybeDynMark (Lev1DynR v) = Just $ DmLev1R v
maybeDynMark (Lev2DynL sign v) = Just $ DmLev2L (sign*v)
maybeDynMark (Lev2DynR sign v) = Just $ DmLev2R (sign*v)
maybeDynMark _ = Nothing


dynDirectionsToDynMark :: Map Loc [Dynamic] -> Map Loc [DynMark]
dynDirectionsToDynMark = lMapMaybe g
  where
    g (SimpleDyn v _) = Just $ DmLev1Simple $ fromIntegral v
    g _ = Nothing
  

-- staffLev1
--
--   Compute Curve representing dynamics on one staff.
--
--   This will first produce Map Loc Lev1Dyn as follows:
--
--     This will combine (1) XML dynamic directions, (2) XML words for
--     dynamics (marks below the staff), (3) hairpins
--
--   Then it will produce curve segments from the Map Loc Lev1Dyn.
--
staffLev1 :: TimeSigs -> AbsTimeMap -> Map Loc [DynMark] -> Staff -> OneCurve
staffLev1 timeSigs atm dynMap staff = error "foo"
    buildCurve timeSigs atm $ mapMaybe doLoc $ M.keys dynMap
  where
    maxTrueEndLoc = error "foo"
    doLoc :: Loc -> Maybe (Loc,(Loc,(Double,Double)))
    doLoc loc = case lev1AtLoc (dLookup loc dynMap) of
      FoldLev1 (Just simpleLev) Nothing ->
        case searchMapFw simpleDyn_fwSearch dynMap loc of
          Nothing         -> Just (loc,(maxTrueEndLoc,(simpleLev,simpleLev)))
          Just (endLoc,_) -> Just (loc,(endLoc,(simpleLev,simpleLev)))
      FoldLev1 Nothing (Just rampBegin) ->
        case searchMapFw fwSearch_lev1R dynMap loc of
          Nothing -> throwMine "2n7cctj"
          Just (endLoc,endVal) -> Just (loc,(endLoc,(rampBegin,endVal)))


simpleDyn_fwSearch (DmLev1L _)      = SiFound ()
simpleDyn_fwSearch (DmLev1R _)      = SiError "akn365"
simpleDyn_fwSearch (DmLev1Simple _) = SiFound ()
simpleDyn_fwSearch _                = SiNonApplicable


fwSearch_lev1R (DmLev1R v)      = SiFound v
fwSearch_lev1R (DmLev1L _)      = SiError ",m359s8"
fwSearch_lev1R (DmLev1Simple _) = SiError ",n21mm9"
fwSearch_lev1R _                = SiNonApplicable


-- FoldLev1: <maybe simple> <maybe left ramp>
data FoldLev1 = FoldLev1 (Maybe Double) (Maybe Double)


lev1AtLoc :: [DynMark] -> FoldLev1
lev1AtLoc ms = foldl go (FoldLev1 Nothing Nothing) ms
  where
    go :: FoldLev1 -> DynMark -> FoldLev1
    go (FoldLev1 m1 m2) (DmLev1Simple v) = FoldLev1 (Just v) m2
    go (FoldLev1 m1 m2) (DmLev1L v)      = FoldLev1 m1 (Just v)
    go fd _ = fd
      

staffLev2 :: TimeSigs -> AbsTimeMap -> Map Loc [DynMark] -> Staff -> OneCurve
staffLev2 = error "foo"

    
dLookup :: Ord k => k -> Map k a -> a
dLookup k m = case M.lookup k m of {Just x -> x}
