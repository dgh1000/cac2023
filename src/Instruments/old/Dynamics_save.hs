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
        lev1OneCurve = staffLev1 (scTimeSigs score) atm staffMarks staff
        lev2OneCurve = staffLev2 (scTimeSigs score) atm staffMarks staff


data SearchItem a = SiFound a
                  | SiNonApplicable
                  | SiError


searchMap :: Ord a => (a -> SearchItem b) -> Map k [a] -> k -> a
searchMap g


-- L1Dyn <dynamic level> <is ramp?> <maybe hairpin sharing this Loc>
-- L1Hairpin <hairpin data>
data Lev1Dyn = L1Dyn Double (Maybe HairpinData)
             | L1DynRamp (Maybe Double) (Maybe Double)
             | L1Hairpin HairpinData
             deriving(Show)

data Lev1RampData = Lev1RampData (Maybe Double) (Maybe Double)


data HairpinData = HairpinData HairpinType Loc
                 deriving(Show)


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
staffLev1 :: TimeSigs -> AbsTimeMap -> Map Loc [MarkD] -> Staff -> OneCurve
staffLev1 timeSigs atm staffMarks staff =
    buildCurve timeSigs atm $ map doLoc (M.keys l1Both)
  where
    
    -- this section computes 'l1Both', the map of Lev1Dyn data that combines
    -- XML dynamics directions, XML words, and hairpins.
    lev1Map = mkLev1DynMap staff staffMarks
    
    -- this section produces [(Loc,(Loc,(Double,Double)))] to pass to
    -- Curves.hs:buildCurve
    --
    -- so does every loc create a segment spanning to next loc? or to max true
    -- end? yes
    doLoc :: Loc -> (Loc,(Loc,(Double,Double)))
    doLoc loc = case M.lookup loc l1Both of
      Just (L1Dyn level Nothing)      ->
        let (end,_) = g level in (loc,(end,(level,level)))
      Just (L1DynRamp _ (Just )     -> 
        let (end,_) = g (Just level) in (loc,(end,(level,level)))
      Just (L1Dyn _ level False (Just hp)) ->
        doHairpin loc level hp (g $ Just level)
      Just (L1Hairpin hpData) -> doHairpin loc priLevel hpData (g Nothing)
        where priLevel = lookupPrior loc
      where
        g level = lookupFollowing loc level
    -- lookup following is meant to 
    lookupFollowing :: Loc -> Maybe Double -> (Loc,Double)
    lookupFollowing loc mCurrLevel = case M.lookupGT loc l1Both of
      Just (end,L1Dyn Nothing endLevel _ _)  -> (end,endLevel)
      Just (end,L1Dyn (Just endLevel) _ _ _) -> (end,endLevel)
      Just (end,L1Hairpin _) -> case mCurrLevel of
        Just currLevel -> (end,currLevel)
      Nothing -> case mCurrLevel of
        Just currLevel -> (endGuess,currLevel)
          where
            oneMsrPast = Loc (msrNum loc + 1) (beat loc)
            endGuess = max oneMsrPast (stMaxTrueEnd staff)
    lookupPrior :: Loc -> Double
    lookupPrior loc = case M.lookupLT loc l1Both of
      Just (_,L1Dyn _ priLevel False Nothing) -> priLevel
      Just (_,L1Dyn _ _ True _) -> throwMine $ printf ("Error at hairpin " ++
        "at %s: prior level 1 dynamic marking must not be ramp")
        (showLoc2 loc)
      
    
doHairpin :: Loc -> Double -> HairpinData -> (Loc,Double) ->
             (Loc,(Loc,(Double,Double)))
doHairpin loc beginLev (HairpinData typ hpEnd) (followingLoc,followingLevel)
  | msrNum hpEnd >= msrNum followingLoc - 1 = go
  | otherwise = throwMine $ printf ("error in hairpin at %s: ending is not " ++
                "within one measure of following dynamic") (showLoc2 loc)
  where
    err2 = throwMine $ printf ("error in hairpin at %s: direction " ++
           "(cresc or dim) is not consistent with dynamics at the " ++
           "ends") (showLoc2 loc)
    go | beginLev >= followingLevel && typ == Crescendo  = err2
       | beginLev <= followingLevel && typ == Diminuendo = err2
       | otherwise = (loc,(followingLoc,(beginLev,followingLevel)))
  

mkLev1DynMap :: Staff -> Map Loc [MarkD] -> Map Loc Lev1Dyn
mkLev1DynMap staff marks = error "foo"
  where
    l1Hps = mkLev1DynMap_hairpin $ stHairpins staff
    l1DynMarks = mkLev1DynMap_marks staff staffMarks
    l1Both :: Map Loc Lev1Dyn
    l1Both = M.unionWith u l1DynMarks l1Hps
    u :: Lev1Dyn -> Lev1Dyn -> Lev1Dyn
    u (L1Dyn mLeft amt rampFlag Nothing) (L1Hairpin d) =
        (L1Dyn mLeft amt rampFlag (Just d))
    


mkLev1DynMap_hairpin :: Map Loc Hairpin -> Map Loc Lev1Dyn
mkLev1DynMap_hairpin = M.map g
  where
    g :: Hairpin -> Lev1Dyn
    g (Hairpin typ end) = L1Hairpin (HairpinData typ end)


-- mkLev1DynMap_marks
--
--   Compute **Map Loc Lev1Dyn** map from (1) XML dynamic directions, (2) XML
--   word dynamic marks (such as ff*)
--
--   (1) Comes from the staff field Map Loc [Dynamic]. This must be converted
--       to Map Loc Lev1Dyn, called map A.
--
--   (2) Comes from 'marks' input. This must be converted to Map Loc
--       Lev1Dyn. Call this map B.
--
--   (3) must find the union of A and B with a function that gives an error
--   when there is a value at the same key at both maps
--
computeLev1Dyn_marks :: Staff -> Map Loc [MarkD] -> Map Loc Lev1Dyn
computeLev1Dyn_marks staff marks = M.unionWithKey err ms1 ms2
  where
    err loc _ _ = throwMine $ printf ("error at %s: both an XML direction-" ++
      "type dynamic and a word custom dynamic are present") (showLoc2 loc)
    -- ms1: map made from XML dynamic "directions"
    ms1 :: Map Loc Lev1Dyn
    ms1 = M.mapMaybeWithKey g $ stDynamics staff
    g :: Loc -> [Dynamic] -> Maybe Lev1Dyn
    g loc ds = case map h ds of
      []  -> Nothing
      [x] -> Just x
     where
       h :: Dynamic -> Lev1Dyn
       h (SimpleDyn lev vn) = L1Dyn (fromIntegral lev) Nothing

    -- ms2: map made from Marks, specifically Lev1DynL or Lev1DynR
    ms2 :: Map Loc Lev1Dyn
    ms2 = M.mapMaybeWithKey g2 marks
    g2 :: Loc -> [MarkD] -> Maybe Lev1Dyn
    g2 loc ms = case mconcat $ mapMaybe maybeLev1RampData ms of
      Lev1RampData Nothing Nothing -> Nothing
      Lev1RampData x y             -> Just $ L1DynRamp x y

        
maybeDynMark :: MarkD -> Maybe Lev1Dyn
maybeDynMark (Lev1DynL mLeft value flag) =
  Just $ L1Dyn mLeft value flag Nothing
maybeDynMark _ = Nothing


combineMaybe :: Maybe a -> Maybe a -> Maybe a
combineMaybe Nothing (Just x) = Just x
combineMaybe (Just x) _ = Just x


instance Monoid Lev1RampData where
  mempty = Lev1RampData Nothing Nothing
  mappend (Lev1RampData x1 y1) (Lev1RampData x2 y2) =
    Lev1RampData (combineMaybe x1 x2) (combineMaybe y1 y2)


maybeLev1RampData :: MarkD -> Maybe Lev1RampData
maybeLev1RampData (Lev1DynL v) = Just $ Lev1RampData Nothing (Just v)
maybeLev1RampData (Lev1DynR v) = Just $ Lev1RampData (Just v) Nothing

        
dumpLev1Dyn :: Map Loc Lev1Dyn -> Map Loc Lev1Dyn -> IO (Map Loc Lev1Dyn)
dumpLev1Dyn dumpMap outMap = do
  let g (loc,d) = printf "%s: %s\n" (showLoc2 loc) (show d)
  writeFile "dynmap.txt" $ concatMap g $ M.toAscList dumpMap
  return outMap



dumpCurves :: Map String Curve -> IO (Map String Curve)
dumpCurves m = do
  let g :: (String,Curve) -> ShowItem
      g (name,c) = Component name True [showI c]
  writeFile "curves.txt" $ showiToString $ Component "" False $
    map g $ M.toAscList m
  return m





----------------------------------------------------------------------
----------------------------------------------------------------------


data Lev2Dyn = L2Left Double
             | L2LeftRight Double Double
             | L2Right Double
             deriving(Show)


data Lev2Dyn2 = L2 (Maybe Double) (Maybe Double)

{-

staffLev2 :: TimeSigs -> AbsTimeMap -> Map Loc [MarkD] -> Staff -> OneCurve
staffLev2 timeSigs atm staffMarks staff =
    buildCurve timeSigs atm $ mapMaybe doLoc $ M.keys l2Map
  where
    l2Map = computeL2Map staffMarks
    doLoc :: Loc -> Maybe (Loc,(Loc,(Double,Double)))
    doLoc loc = case M.lookup loc l2Map of
      Just (L2 _ (Just beg)) -> Just $ doLeft loc beg
      _                      -> Nothing
    doLeft :: Loc -> Double -> (Loc,(Loc,(Double,Double)))
    doLeft loc begValue = case M.lookupGT loc l2Map of
      Just (locE,L2 (Just endValue) _) -> (loc,(locE,(begValue,endValue)))
      Just (_   ,L2 Nothing         _) -> err
      Nothing                          -> err
      where
        err = throwMine $ printf
              "error in beginning level 2 dynamic at %s: no ending"
              (showLoc2 loc)
    

computeL2Map :: Map Loc [MarkD] -> Map Loc Lev2Dyn2
computeL2Map staffMarks = M.mapMaybeWithKey g staffMarks
  where
    g :: Loc -> [MarkD] -> Maybe Lev2Dyn2
    g loc ms = case mapMaybe h ms of
      []  -> Nothing
      [x] -> Just x
    h :: MarkD -> Maybe Lev2Dyn2
    h (Lev2DynLeft sign amt) =
      Just $ L2 Nothing (Just $ sign*amt)
    h (Lev2DynLeftRight sign amt) =
      Just $ L2 (Just $ sign*amt) (Just $ sign*amt)
    h (Lev2DynRight sign amt) =
      Just $ L2 (Just $ sign*amt) Nothing
    h _ = Nothing
    

dLookup :: Ord k => k -> Map k a -> a
dLookup k m = case M.lookup k m of {Just x -> x}

-}

