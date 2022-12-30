{-# LANGUAGE TupleSections #-}
module Instruments.Dynamics where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import qualified Common as CD
import Control.Monad.State
import Control.Arrow
import Debug.Trace
import Text.Printf
import Data.Maybe
import Data.Map.Strict(Map)
import Data.Ratio
import Common
import Common.CommonUtil
import Score.ScoreExport
import Score.ScoreData
import Instruments.TimeMap
import Instruments
import Instruments.Curves
import Instruments.InstrumentsData
import Util.Exception
import Util.Math(scale)


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


hpDynCurves :: Map String AbsTimeMap -> Score -> Map String Curve
hpDynCurves atms score = M.map f $ scStaves score
  where
    f :: Staff -> Curve
    f staff = let atm = dLookup (stName staff) atms
              in Curve [hpDynCurve (scTimeSigs score) atm staff]


dLookup :: Ord k => k -> Map k a -> a
dLookup k m = case M.lookup k m of {Just x -> x}


hpDynCurve :: TimeSigs -> AbsTimeMap -> Staff -> OneCurve
hpDynCurve timeSigs atm
           Staff { stHairpins    = hairpins
                 , stDynamics    = dynamics
                 , stMaxTrueEnd  = eMax } =
    foldl step (doDynamics timeSigs atm eMax dynamics) $ M.toAscList hairpins
  where
    -- we need to change this to use 
    step :: OneCurve -> (Loc,Hairpin) -> OneCurve
    step (OneCurve c ts tm) (locH1,Hairpin _ locH2) =
      let rH1 = lookupTime locH1 atm
          rH2 = lookupTime locH2 atm
          s1 = M.lookupLE rH1 c
          lOf (_,Seg _ x _) = x 
          mLoud2 = lOf <$> (s1 >>= flip M.lookupGT c . fst)          
      in case (s1,mLoud2) of
        (Just (x1,Seg x2 loud1 _),Just z)
             -> OneCurve (insertHp x1 x2 rH1 rH2 locH1 locH2 loud1 z c) ts tm
        _    -> throwMine $ printf ("something wrong in hairpin at %s; " ++
                "it needs to be situated entirely between dynamic marks")
                (showLoc2 locH1)


-- <beg/end time of segment into which we are inserting>
-- <beg/end time of hairpin>
-- <beg/end Loc of hairpin>
-- <beg/end loudness>
data HairpinInsert = HairpinInsert (Double,Double) (Double,Double) (Loc,Loc)
                     (Double,Double) (Map Double Seg)
        
{-               
      let s1 = M.lookupLE locH1 c
          mLoud2 = lOf <$> (s1 >>= flip M.lookupGT c . fst)
          lOf (_,(_,(x,_))) = x
      in case (s1,mLoud2) of
           (Just (loc1,(loc2,(loud1,_))),Just z)
             -> insertHp loc1 loc2 locH1 locH2 loud1 z c
           _ -> throwMine $ printf ("something wrong in hairpin at %s; " ++
                "it needs to be situated entirely between dynamic marks")
                (showLoc2 locH1)
-}

insertHp :: Double -> Double -> Double -> Double -> Loc -> Loc ->
            Double -> Double -> Map Double Seg -> Map Double Seg
insertHp segL1 segL2 hpL1 hpL2 hpLoc1 hpLoc2 loud1 loud2 c
  -- in this case, hairpin is exactly coincident with segment: we insert
  -- one new segment
  | segL1 == hpL1 && segL2 == hpL2 =
      M.insert segL1 (Seg segL2 loud1 loud2) c2
  -- in this case, haipin end coincides with segment end; we insert two
  -- segments, a flat one to the left and ramped one to the right
  | segL1 < hpL1 && segL2 == hpL2 =
      M.insert segL1 (Seg hpL1  loud1 loud1) .
      M.insert hpL1  (Seg segL2 loud1 loud2) $ c2
  -- here hairpin beg coincides with segment beg; we still do the old strategy
  -- of assuming hairpin ends right at new dynamic, so we just insert one
  -- segment. Also we assure that hairpin ends within 3 seconds of new
  -- dynamic.
  | segL1 == hpL1 && hpL2 < segL2 && hpL2 >= segL2-3 =
      M.insert segL1 (Seg segL2  loud1 loud2) $ c2
  -- here hairpin starts after the segment and ends before the segment end.
  | segL1 < hpL1 && hpL2 < segL2 && hpL2 >= segL2-3 =
      M.insert segL1 (Seg hpL1  loud1 loud1) .
      M.insert hpL1  (Seg segL2 loud1 loud2) $ c2
  | otherwise = throwMine msg 
  where
    msg = printf ("check hairpin spanning %s -> %s. Hairpin should be " ++
          "contained within dynamic marks and within three seconds of " ++
          "the ending mark") (showLoc2 hpLoc1) (showLoc2 hpLoc2)
    c2 = M.delete segL1 c



doDynamics :: TimeSigs -> AbsTimeMap -> Loc -> Map Loc [Dynamic] -> OneCurve
doDynamics timeSigs atm end dyns
  | null z = throwMine "at least one dynamic must be on each staff"
  | otherwise = buildCurve timeSigs atm $ map g z
  where
    l = (M.toAscList $ M.mapWithKey toOneDyn $ dyns) ++ [(end,SimpleDyn 4 1)]
    z = zip l (tail l)
    toOneDyn :: Loc -> [Dynamic] -> Dynamic
    toOneDyn loc ds = case ds of
      [x] -> x
      _   -> throwMine $ printf "at %s, there are 2 or more dynamic marks"
               (showLoc2 loc)
    g :: ((Loc,Dynamic),(Loc,Dynamic)) -> (Loc,(Loc,(Double,Double)))
    g ((loc1,SimpleDyn l _),(loc2,_))
        = (loc1,(loc2,(fromIntegral l,fromIntegral l)))




----------------------------------------------------------------------
----------------------------------------------------------------------

----------------------------------------------------------------------
----------------------------------------------------------------------
--                    ADD SEGMENTS





----------------------------------------------------------------------
----------------------------------------------------------------------


