{-# LANGUAGE TupleSections #-}
module Translation.Dynamics where

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
import Translation.TimeMap
import Translation
import Translation.Curves
import Translation.ShowTranslation
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

-- MUSESCORE_UPGRADE
hpDynCurves :: Map String AbsTimeMap -> Score -> Map String [Curve]
hpDynCurves atms score = M.mapWithKey doPartStaff (scStaves score)
  where
    -- we need to get each staff/part
    ts = scTimeSigs score
    doPartStaff :: String -> Staff -> [Curve]
    doPartStaff staffN staff = hpDynCurvesOnePart ts atm mte dyns hps
      where
        atm = case M.lookup staffN atms of {Just x -> x}
        mte = stMaxTrueEnd staff
        dyns = stDynamics staff
        hps = stHairpins staff


hpDynCurvesOnePart :: TimeSigs -> AbsTimeMap -> Loc -> [Map Loc Dynamic] -> [Map Loc Hairpin] ->
                      [Curve]
hpDynCurvesOnePart ts atm endLoc = zipWith (hpDynCurvesOneScoreStaff ts atm endLoc)
    
-- hpDynCurvesOneScoreStaff
--  TimeSigs
--  AbsTimeMap
--  <dynamics one score staff>
--  <hairpins one score staff>
--  <max true end loc>
--  -> 
hpDynCurvesOneScoreStaff :: TimeSigs -> AbsTimeMap -> Loc -> Map Loc Dynamic -> Map Loc Hairpin ->
                            Curve
hpDynCurvesOneScoreStaff ts atm maxTrueEnd dynamics hairpins = Curve [lev1OneCurve]
  where
    dynMarks1 :: Map Loc DynMark
    dynMarks1 = dynDirectionsToDynMark dynamics
    dynMarks2 :: Map Loc [DynMark]
    dynMarks2 = M.map (\x -> [x]) dynMarks1
    hpAsDyns = M.map g hairpins
    g :: Hairpin -> [DynMark]
    g (Hairpin typ end sNum) = [DmHairpin typ end sNum]
    allDyns = M.unionsWith (++) [dynMarks2,hpAsDyns]
    lev1OneCurve = staffLev1 ts atm allDyns maxTrueEnd


{-
splitDynMarks :: Map Loc [DynMark] -> [Map Loc [DynMark]]
splitDynMarks dyns = error "foo"


makeCurvesOneScoreStaff :: Map String AbsTimeMap -> Map Loc [DynMark] -> Loc -> Curve
makeCurvesOneScoreStaff atm dyns maxTrueEnd = error "foo"
-}

{-
hpDynCurves :: Map String AbsTimeMap -> Score -> Map String Curve
hpDynCurves atms score =  M.map f $ scStaves score
  -- unsafePerformIO (dumpCurves $ M.map f $ scStaves score)
  where
    f :: Staff -> Curve
    f staff = Curve [lev1OneCurve,lev2OneCurve]
      where
        staffMarks = dLookup (stName staff) (scMarksByStaff score)
        atm = dLookup (stName staff) atms
        dynMarks1 = lMapMaybe maybeDynMark staffMarks
        dynMarks2 = dynDirectionsToDynMark $ stDynamics staff
        dynMarks = integrateDynMarks dynMarks1 dynMarks2 (stHairpins staff)
        lev1OneCurve = staffLev1 (scTimeSigs score) atm dynMarks staff 
        lev2OneCurve = staffLev2 (scTimeSigs score) atm dynMarks staff
-}

integrateDynMarks :: Map Loc [DynMark] -> Map Loc [DynMark] ->
                     Map Loc Hairpin -> Map Loc [DynMark]
integrateDynMarks dyns1 dyns2 hps = dynPre
  where
    dynPre = M.unionsWith (++) [dyns1,dyns2,M.map g hps]
    g :: Hairpin -> [DynMark]
    g (Hairpin typ end sNum) = [DmHairpin typ end sNum]


dumpCurves :: Map String Curve -> IO (Map String Curve)
dumpCurves cs = do
  let g (name,c) = Component name True [showI c]
  putStrLn "writing curves"
  writeFile "curves.txt" $ showiToString $ Component "" False
                           (map g $ M.toAscList cs)
  return cs


data DynMark = DmLev1Simple Double Int -- <loudness> <staff num>
             | DmLev1R Double
             | DmLev1L Double
             | DmLev1LR Double Double
             | DmLev2R Double
             | DmLev2L Double
             | DmLev2LR Double
             | DmHairpin HairpinType Loc Int -- <type> <location> <staff number>


maybeDynMark :: MarkD -> Maybe DynMark
maybeDynMark (Lev1DynL v) = Just $ DmLev1L v
maybeDynMark (Lev1DynR v) = Just $ DmLev1R v
maybeDynMark (Lev1DynLR v1 v2) = Just $ DmLev1LR v1 v2 
maybeDynMark (Lev2DynL sign v) = Just $ DmLev2L (sign*v)
maybeDynMark (Lev2DynR sign v) = Just $ DmLev2R (sign*v)
maybeDynMark (Lev2DynLR sign v) = Just $ DmLev2LR (sign*v)
maybeDynMark _ = Nothing


dynDirectionsToDynMark :: Map Loc Dynamic -> Map Loc DynMark
dynDirectionsToDynMark = M.fromList . mapMaybe g . M.toList
  where
    g (k,SimpleDyn v staffNum) = Just $ (k,DmLev1Simple (fromIntegral v) staffNum)
    g _ = Nothing
  

-- FoldLev1: <maybe simple> <maybe left ramp> <maybe right ramp>
--           <maybe hairpin end loc>
data FoldLev1 = FoldLev1
  { fl1Simple  :: Maybe Double
  , fl1RampL   :: Maybe Double
  , fl1RampR   :: Maybe Double
  , fl1Hairpin :: Maybe (HairpinType, Loc)
  }
              deriving(Eq)


showFoldLev1 :: FoldLev1 -> ShowItem
showFoldLev1 (FoldLev1 simp rampL rampR hp) =
  Component (printf "simple: %s" $ show $ fmap g simp) True
    [out1,out2]
  where
    out1 = SingleLine $ printf "ramp left: %s ramp rght: %s"
                       (show $ fmap g rampL)
                       (show $ fmap g rampR)
    out2 = SingleLine $ printf "hairpin  : :%s" (show $ fmap showHp hp)
    g :: Double -> String
    g d = printf "%.2f" d
    showHp :: (HairpinType, Loc) -> String
    showHp (typ,loc) = printf "(hp %s %s)" (show typ) (showLoc2 loc)


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

staffLev1 :: TimeSigs -> AbsTimeMap -> Map Loc [DynMark] -> Loc -> OneCurve
staffLev1 timeSigs atm dynMap maxTrueEnd =
    let
      out = buildCurve timeSigs atm $ map doLoc2 $ M.keys m2
    in
      out
  where
    mte (Loc m b) = max maxTrueEnd (Loc (m+1) b)
    m2 :: Map Loc FoldLev1
    m2 = M.mapMaybeWithKey fold1Loc dynMap
    doLoc2 loc1 = staffLev1_doOneLoc m2 maxTrueEnd loc1
    
    {-
    doLoc :: Loc -> (Loc,(Loc,(Double,Double)))
    doLoc loc = case ddLookup loc m2 of
      FoldLev1 (Just simpleLev) Nothing _ Nothing ->
        case M.lookupGT loc m2 of
          Just (endLoc,_) -> (loc,(endLoc,(simpleLev,simpleLev)))
          -- must be at least one measure greater than loc
          Nothing         -> (loc,(mte loc,(simpleLev,simpleLev)))
      FoldLev1 _ Nothing _ (Just (typ,end)) ->
        doHairpin m2 loc typ end
      FoldLev1 Nothing (Just rampBegin) _ Nothing ->
        case M.lookupGT loc m2 of
          Just (endLoc,FoldLev1 _ _ (Just rampEnd) _) ->
            (loc,(endLoc,(rampBegin,rampEnd)))
          _ -> throwMine $ printf ("error in ramp begin at %s: what follows "++
                                  "it must be a ramp ending") (showLoc2 loc)
      FoldLev1 Nothing Nothing _ _ ->
        throwMine $ printf ("error at %s: neither "++
                            "simple dynamic nor left ramp is present")
                            (showLoc2 loc) 
     -}

-- staffLev1_doOneLoc
--
--   Produce a "dynamic segment", type (Loc,(Loc,(Double,Double))),
--   running from the given loc, 'loc1', to the following loc in the
--   dynamic map in, 'mapIn'
--
staffLev1_doOneLoc :: Map Loc FoldLev1 -> Loc -> Loc ->
                      (Loc,(Loc,(Double,Double)))
staffLev1_doOneLoc mapIn maxTrueEndOfStaff loc1 = out
  where

    -- Create an artificial ending Loc for last segment.
    mkEnding (Loc m b) = max maxTrueEndOfStaff (Loc (m+1) b)
    
    -- Handle all cases of what level 1 dynamic markings are present
    -- at loc1.
    
    out = case ddLookup loc1 mapIn of

      -- Case 1: there is a simple dynamic and no left ramp and no hairpin.
      FoldLev1 (Just simpleLev) Nothing _ Nothing ->
        
        -- Check what Loc follows loc1. Maybe none!
        case M.lookupGT loc1 mapIn of

          -- subcase: loc1 was followed by 'endLoc':
          -- 
          --    produce a simple, flat dynamics segment
          Just (endLoc,_) -> (loc1,(endLoc,(simpleLev,simpleLev)))
          
          -- subcase: loc1 was followed by no higher loc.
          --
          --   create an artificial end loc. must be at least one measure
          --   greater than loc1. produce flat dynamic segment.
          Nothing         -> (loc1,(mkEnding loc1,(simpleLev,simpleLev)))

      -- Case 2. There's a hairpin starting at 'loc1'.
      FoldLev1 _ Nothing _ (Just (typ,end)) -> doHairpin mapIn loc1 typ end

      -- Case 3. A "Level 1 ramp" begins this segment.
      FoldLev1 Nothing (Just rampBegin) _ Nothing ->

        -- check what follows 'loc1'
        case M.lookupGT loc1 mapIn of

          -- subcase: loc1 was followed by end ramp
          Just (endLoc,FoldLev1 _ _ (Just rampEnd) _)
            -> let out = (loc1,(endLoc,(rampBegin,rampEnd)))
               in  out

          -- subcase: no end ramp followed
          _
            -> throwMine $ printf ("error in ramp begin at %s: what follows "++
               "it must be a ramp ending") (showLoc2 loc1)

      -- Case 4. We are going to use the right ramp as beginning of this
      --         segment.
      FoldLev1 _ _ (Just rampEnd) Nothing ->

        -- Check what Loc follows loc1. Maybe none!
        case M.lookupGT loc1 mapIn of

          -- subcase: loc1 was followed by 'endLoc':
          -- 
          --    produce a simple, flat dynamics segment
          Just (endLoc,_) -> (loc1,(endLoc,(rampEnd,rampEnd)))
          
          -- subcase: loc1 was followed by no higher loc.
          --
          --   create an artificial end loc. must be at least one measure
          --   greater than loc1. produce flat dynamic segment.
          Nothing         -> (loc1,(mkEnding loc1,(rampEnd,rampEnd)))
        
      
doHairpin :: Map Loc FoldLev1 -> Loc -> HairpinType -> Loc ->
             (Loc,(Loc,(Double,Double)))
doHairpin m atLoc typ endLoc
  | followLoc < endLoc = throwMine $ printf ("error in hairpin at %s: " ++
                         "hairpin end must be exceed following simple dyn")
                         (showLoc2 atLoc)
  | msrNum endLoc >= msrNum followLoc - 1 =
      (atLoc,(followLoc,(baseValue,followVal)))
  | otherwise = throwMine $ printf ("error in hairpin at %s: following " ++
                "dynamic must not be more than one measure beyond the end " ++
                "of the hairpin") (showLoc2 atLoc)
                
  where
    -- searching for base value
    searchBase_apply (loc,fl1) = case fl1_maybeHpBase fl1 of
      SrError s -> throwMine $ printf ("error at %s while searching for " ++
                   "PREVIOUS dynamic level of hairpin: %s") (showLoc2 atLoc)
                   s
      SrYes x        -> Just x
      SrNotApplicble -> Nothing
    baseValue = case L.find isJust $ map searchBase_apply $
                     M.toDescList $ fst $ splitInclude atLoc m of
      Just (Just x) -> x

    -- searching for follow value
    searchFollow_apply (loc,fl1) = case fl1_maybeHairpinFollow fl1 of
      SrError s ->
        unsafePerformIO (hairpinDump loc m) `seq`
        (throwMine $ printf ("error at %s while searching for " ++
                   "FOLLOW dynamic level of hairpin: %s") (showLoc2 atLoc)
                   s)
      SrYes x        -> Just (loc,x)
      SrNotApplicble -> Nothing
    (followLoc,followVal) = case L.find isJust $ map searchFollow_apply $
                       M.toAscList $ snd $ M.split atLoc m of
      Just (Just x) -> x
      _             -> throwMine $ printf ("some error in hairpin at %s: " ++
                       "maybe no following dynamic?") (showLoc2 atLoc)


hairpinDump :: Loc -> Map Loc FoldLev1 -> IO ()
hairpinDump locIn m = do
  let f (loc,fl) = Component (showLoc2 loc) True [showFoldLev1 fl]
      loc1 = Loc (msrNum locIn - 4) 1
      loc2 = Loc (msrNum locIn + 4) 1
      c = Component "" False $
          map f $ filter (\(l,_) -> loc1 <= l && l <= loc2) $ M.toAscList m
  writeFile "fl1.txt" $ showiToString c
  


data SearchResult a = SrYes a
                    | SrNotApplicble
                    | SrError String


fl1_maybeHairpinFollow :: FoldLev1 -> SearchResult Double
fl1_maybeHairpinFollow (FoldLev1 _ _ (Just _) _) =
  SrError "ramp R cannot be present on right side"
fl1_maybeHairpinFollow (FoldLev1 (Just v) _ _ _) = SrYes v
fl1_maybeHairpinFollow _ = SrError "to right of hairpin, no simple dynamic"


fl1_maybeHpBase :: FoldLev1 -> SearchResult Double
fl1_maybeHpBase (FoldLev1 _        (Just _) _ _) =
  SrError "ramp L cannot be present"
fl1_maybeHpBase (FoldLev1 (Just v) _        _ _) =
  SrYes v
fl1_maybeHpBase _ = SrNotApplicble

                    
fl1_maybeRightRamp :: FoldLev1 -> SearchResult Double
fl1_maybeRightRamp (FoldLev1 _ (Just _) _ _) =
  SrError "another left ramp can't be present"
fl1_maybeRightRamp (FoldLev1 _ _ (Just v) _) = SrYes v
fl1_maybeRightRamp (FoldLev1 _ _ _ (Just _)) =
  SrError "another hairpin can't be present"
fl1_maybeRightRamp _ = SrNotApplicble


-- MUSESCORE just a note to myself: I think this doesn't need
-- to consider staff number because we would have sorted DynMark's before
-- calling this.
fold1Loc :: Loc -> [DynMark] -> Maybe FoldLev1
fold1Loc atLoc ms = case foldl go empty ms of
    f | f == empty -> Nothing
      | otherwise  -> Just f
  where
    empty = FoldLev1 Nothing Nothing Nothing Nothing
    go :: FoldLev1 -> DynMark -> FoldLev1
    go (FoldLev1 m1 m2 m3 m4) (DmLev1Simple v _) =
      FoldLev1 (combMaybe atLoc "simple dyn" m1 v) m2 m3 m4
    go (FoldLev1 m1 m2 m3 m4) (DmLev1L v)      =
      let
        out = (combMaybe atLoc "level 1 left ramp" m2 v)
      in
        FoldLev1 m1 out m3 m4 
    go (FoldLev1 m1 m2 m3 m4) (DmLev1R v)      =
      FoldLev1 m1 m2 (combMaybe atLoc "level 1 right ramp" m3 v) m4
    go (FoldLev1 m1 m2 m3 m4) (DmLev1LR v1 v2) =
      FoldLev1 m1 (combMaybe atLoc "lev1 LR" m2 v2)
                  (combMaybe atLoc "lev1 LR" m3 v1) m4
    go (FoldLev1 m1 m2 m3 m4) (DmHairpin typ end _) =
      FoldLev1 m1 m2 m3 (combMaybe atLoc "hairpin" m4 (typ,end))
    go fd _ = fd


combMaybe :: Loc -> String -> Maybe a -> a -> Maybe a
combMaybe _   _   Nothing  x = Just x
combMaybe loc err (Just _) _ = throwMine $ printf ("error at %s: more than "++
                               "one %s") (showLoc2 loc) err


----------------------------------------------------------------------
----------------------------------------------------------------------
--                  LEVEL 2



data FoldLev2 = FoldLev2 (Maybe Double) (Maybe Double)
  deriving(Eq)


  

staffLev2 :: TimeSigs -> AbsTimeMap -> Map Loc [DynMark] -> Staff -> OneCurve
staffLev2 timeSigs atm dynMarks staff =
    buildCurve timeSigs atm $ mapMaybe doLoc $ M.keys fl2Map 
  where
    fl2Map = M.mapMaybeWithKey fold2Loc dynMarks
    doLoc :: Loc -> Maybe (Loc,(Loc,(Double,Double)))
    doLoc loc = case M.lookup loc fl2Map of
      Just (FoldLev2 (Just v) _) -> case M.lookupGT loc fl2Map of
        Just (followLoc,FoldLev2 _ (Just followV)) ->
          Just (loc,(followLoc,(v,followV)))
        _ -> throwMine $ printf ("error in level 2 ramp (LEFT) at %s: " ++
             "not followed by right ramp")
      _ -> Nothing


fl2_maybeRight :: FoldLev2 -> SearchResult Double
fl2_maybeRight (FoldLev2 _ (Just v)) = SrYes v
fl2_maybeRight _ = SrError "no level 2 RIGHT ramp"


fold2Loc :: Loc -> [DynMark] -> Maybe FoldLev2
fold2Loc atLoc ms = case foldl go empty ms of
  f | f == empty -> Nothing
    | otherwise  -> Just f
  where
    empty = FoldLev2 Nothing Nothing
    go :: FoldLev2 -> DynMark -> FoldLev2
    go (FoldLev2 m1 m2) (DmLev2L v) =
      FoldLev2 (combMaybe atLoc "level 2 left ramp" m1 v) m2
    go (FoldLev2 m1 m2) (DmLev2R v) =
      FoldLev2 m1 (combMaybe atLoc "level 2 right ramp" m2 v)
    go (FoldLev2 m1 m2) (DmLev2LR v) =
      FoldLev2 (combMaybe atLoc "level 2 cominbation left/right" m1 v)
               (combMaybe atLoc "level 2 cominbation left/right" m2 v)
    go fd _ = fd


dLookup :: Ord k => k -> Map k a -> a
dLookup k m = case M.lookup k m of {Just x -> x}

ddLookup k m = case M.lookup k m of {Just x -> x}
