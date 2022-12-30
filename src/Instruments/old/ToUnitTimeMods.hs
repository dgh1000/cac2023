
module Instruments.ToUnitTimeMods where


-- boundaries/adjusts: how do I know which direction to ramp? 1 on the side of
-- smaller absolute adjust?



import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.List as L
import Debug.Trace
import Control.Monad.State
import Text.Printf
import Control.Monad
import Data.Either
import Data.Map(Map)
import Data.Set(Set)
import Data.Maybe
import Data.Monoid
import Score.ScoreData
import Instruments
import Instruments.InstrumentsData
import Common
import Common.CommonUtil
import Util.Exception
import Util.Map
import Data.Ratio

-- can these be expressed in unit time mods? something like accel a certain
-- amount but adjust for absolute time that must be matched.

-- also pause is elemental.

-- we need to decide order of time mods for instance. 

-- we sometimes need to look up w's or other markers. on first pass pair
-- certain Marks with w's or whatever

-- it would be helpful to sort Marks first or extract

-- pair every mark with


-- things that are changed to unit time mods

-- staffAdjusts, globalAdjusts, warps, abs warps, ramps


-- how do I tweak ramps? similar

-- need to find 

----------------------------------------------------------------------
----------------------------------------------------------------------


{-

utmGlob :: Score -> Map Loc (Map String [TimeModMark])
utmGlob score marks = []
  where
    context = Context2 (M.keys $ scStaves score) (scTimeSigs score)
              (scMarkers score)
    -- we need to compute:
    --
    --  global boundaries/adjusts
    --
    --    UnitWarp2 for all staves 
    -- 
    --  abs warp, which means referring to w markers
    --
    --    UnitAbsWarp
    --
    --  pause
    --
    --    UnitPause
    --
    --  staff boundaries/adjusts
-}


---------------------------------------------------------------------------
---------------------------------------------------------------------------
---------------------------------------------------------------------------
--                  code for handling PAUSES


pauses :: Map Loc (Map String [TimeModMark]) -> [UnitTimeMod]
pauses m = map toUtm $ M.toAscList pauseMap
  where
    pauseMap :: Map Loc Double
    pauseMap = M.mapMaybeWithKey xx m
      where
        xx :: Loc -> Map String [TimeModMark] -> Maybe Double
        xx loc1 m = f loc1 . concat $ M.elems m
        f :: Loc -> [TimeModMark] -> Maybe Double
        f loc1 ms = case mapMaybe g ms of
          []    -> Nothing
          [amt] -> Just amt
          _     -> throwMine $ printf ("multiple pauses at %s, " ++
                                  "perhaps on different staves")
                                  (showLoc2 loc1)
          where g :: TimeModMark -> Maybe Double
                g (TmmPause _ d) = Just d
                g _              = Nothing
    -- making Map Loc Double into [UnitTimeMod]
    toUtm :: (Loc,Double) -> UnitTimeMod
    toUtm (loc1,d) = UnitPause loc1 (approxRational d 0.001)


---------------------------------------------------------------------------
---------------------------------------------------------------------------
--              handling AFTERPAUSES


afterPauses :: Map Int TimeSig ->
               Map Loc (Map String [TimeModMark]) ->
               [UnitTimeMod]
afterPauses timeSigs m = map toUtm $ M.toAscList apauseMap
  where
    apauseMap :: Map Loc Double
    apauseMap = M.mapMaybeWithKey xx m 
      where
        -- xx looks at TimeModMarks on all staves and
        -- maps f over them
        xx :: Loc -> Map String [TimeModMark] -> Maybe Double 
        xx loc1 m = f loc1 . concat $ M.elems m
        -- f maps 'g' to get a single after pause 
        f loc1 ms = case mapMaybe g ms of
          []    -> Nothing
          [x]   -> Just x
          _     -> throwMine $ printf "multiple after pauses at %s"
                                      (showLoc2 loc1)
          where
            g (TmmAfterPause _ a) = Just a 
            g _                   = Nothing
    toUtm :: (Loc,Double) -> UnitTimeMod
    toUtm (loc,d) = UnitPostPause loc (approxRational d 0.001)

{-

afterPauses_ :: Map Int TimeSig ->
           Map Loc (Map String [TimeModMark]) ->
           [UnitTimeMod]
afterPauses_ timeSigs m = map toUtm $ M.toAscList apauseMap
  where
    apauseMap :: Map Loc (Char,Double)
    apauseMap = M.mapMaybeWithKey xx m
      where
        xx loc1 m = f loc1 . concat $ M.elems m
        f loc1 ms = case mapMaybe g ms of
          []    -> Nothing
          [x]   -> Just x
          _     -> throwMine $ printf "multiple after pauses at %s"
                                      (showLoc2 loc1)
          where
            g (TmmAfterPause typ _ a) = Just (typ,a)
            g _                       = Nothing
    toUtm :: (Loc, (Char,Double)) -> UnitTimeMod
    toUtm (loc1,(c,d)) = case c of
      'A' -> apF1 timeSigs loc1 d
      'B' -> apF2 timeSigs loc1 d
      
-}      

{-
apGen :: ApFunc -> Map Int TimeSig ->
         Map Loc (Map String [TimeModMark]) ->
         [UnitTimeMod]
apGen apf timeSigs m = map toUtm $ M.toAscList apauseMap
  where
    apauseMap :: Map Loc Double
    apauseMap = M.mapMaybeWithKey xx m
      where
        xx loc1 m = f loc1 . concat $ M.elems m
        f loc1 ms = case mapMaybe g ms of
          [] -> Nothing
          [amt] -> Just amt
          _     -> throwMine $ printf "multiple after pauses at %s"
                                      (showLoc2 loc1)
          where
            g (TmmAfterPause _ a) = Just a
            g _                   = Nothing
    toUtm :: (Loc, Double) -> UnitTimeMod
    toUtm (loc1,d) = apf timeSigs loc1 d
-}      


type ApFunc = TimeSigs -> Loc -> Double -> UnitTimeMod

apF1 :: ApFunc
apF1 timeSigs loc1 amt = Unit2Modify loc0 loc1 loc2 tm01 tm12 Nothing
  where
    -- half a quarter, 0.5 / (0.5 + d)
    ratioSlowDown = 0.5 / (0.5+amt)
    loc0 = case locAddQuar timeSigs loc1 (-(1%2)) of {Just x -> x}
    loc2 = case locAddQuar timeSigs loc1 (1%2) of {Just x -> x}
    tm01 = TmRamp 1 1.1
    tm12 = TmRampParab ratioSlowDown 1 True


apF2 :: ApFunc
apF2 timeSigs loc1 amt = Unit2Modify loc0 loc1 loc2 tm01 tm12 Nothing
  where
    -- half a quarter, 0.5 / (0.5 + d)
    ratioSlowDown = 0.5 / (0.5+amt)
    loc0 = case locAddQuar timeSigs loc1 (-(1%2)) of {Just x -> x}
    loc2 = case locAddQuar timeSigs loc1 (1%2) of {Just x -> x}
    tm01 = TmRamp 1 0.8
    tm12 = TmRampParab ratioSlowDown 1 True



---------------------------------------------------------------------------
---------------------------------------------------------------------------
---------------------------------------------------------------------------
--                   code for handling BOUNDARIES/ADJUSTS


-- boundaries
--
--   Take TimeModMark map (*) and produce several sets of UnitTimeMod for
--   handling the BOUNDARIES/ADJUSTS. The sets include the GLOBAL
--   UnitTimeMods, and the LOCAL ones for each staff name.
--
--   Output: (<GLOBAL UnitTimeMods>, <map of LOCAL UnitTimeMods>)
--
--   (*) This is constructed from the main Mark3 map for all Locs
--   and staves by filtering out time modification marks
--
-- Convert Map of TimeModMark to two sets of UnitTimeMod: global and local
boundaries :: Map Loc (Map String [TimeModMark]) ->
              ([UnitTimeMod],Map String [UnitTimeMod])
boundaries m = (x1,x2)
  where
    -- This collects and sets up a BoundaryState for each staff.
    -- Each boundary state has a field, tmsBoundaries, which is a list of
    -- all boundary regions as type OneBoundary
    --
    -- OneBoundary:
    --
    --    - first and last Loc
    --    - flag indicating if this is a global boundary
    --    - list of adjusts, which are type (Loc,Double)
    
    
    boundStatesPerStaff :: Map String BoundaryState
    boundStatesPerStaff = boundariesSt m
    
    -- We call 'toUnitTimeMods' on each BoundaryState (i.e. once per staff)
    -- and construct both a list of global time mods and per-staff time mods.
    x :: Map String ([UnitTimeMod],[UnitTimeMod])
    x = M.mapWithKey toUnitTimeMods boundStatesPerStaff
    x1 = concatMap fst $ M.elems x
    x2 = M.map snd x

    -- toUnitTimeMods
    --
    --   Converts all boundaries in one staff (BoundaryState)
    --
    toUnitTimeMods :: String -> BoundaryState -> ([UnitTimeMod],[UnitTimeMod])
    toUnitTimeMods name (BoundaryState _ _ _ bs) = out
      where
        out = mconcat $ map doBoundaryFold bs


doBoundaryFold :: OneBoundary -> ([UnitTimeMod],[UnitTimeMod])
doBoundaryFold ob@(OneBoundary (beg,_) globFlag _ mLastOffset) = out3
  where
    OneBoundaryFold _ _ accum =
      foldl stepOneBoundary (OneBoundaryFold 0 beg []) (oneBoundaryToPairs ob)
    out = [UnitWarp2 accum]
    out3 = if globFlag then (out,[]) else ([],out)


oneBoundaryToPairs :: OneBoundary -> [(Loc,Double)]
oneBoundaryToPairs (OneBoundary (beg,end) _ adjs mLastOffset) =
  L.sort adjs ++ [(end,x)]
  where
    x = case mLastOffset of
      Nothing -> 0
      Just x  -> x


-- 

data OneBoundaryFold = OneBoundaryFold
  { obfLastOffset :: Double
  , obfLastLoc    :: Loc
  , obfAccum      :: [Warp2Data]
  }
  

-- stepOneBoundary
--
-- A structure that is folded over a list of adjusts. We don't know
-- time duration of each warp. Ramp up or down. 
stepOneBoundary :: OneBoundaryFold -> (Loc,Double) -> OneBoundaryFold
stepOneBoundary (OneBoundaryFold lastOffset lastLoc accum) (locIn,amt) =
  OneBoundaryFold amt locIn
                  (Warp2Data lastLoc locIn currOffset rampDir : accum)
  where
    currOffset = amt-lastOffset
    rampDir = if abs lastOffset < abs currOffset
        then RewoEnd
        else RewoBegin



type O = (Loc,Maybe Double)
       
type OneAdjust = (Loc,Double)

data OneBoundary = OneBoundary
  { obLocs    :: (Loc,Loc)
  , obGlob    :: Bool
  , obAdjs    :: [OneAdjust]
  , obLastAdj :: Maybe Double
  }

type OneAbsWarp = ((Loc,Loc),Double)

data BoundaryState = BoundaryState
  { tmsLastBoundary :: Maybe Loc  -- not sure if this is last
                                  -- pipe symbol
  , tmsGlobAdjFlag  :: Bool
  , tmsAccumAdjust  :: [OneAdjust]
  , tmsBoundaries   :: [OneBoundary]
  }


-- showUtm (UnitWarp2 beg end amt _) =
--  printf "UnitWarp2: %s %s %10.4f"  (showLoc2 beg) (showLoc2 end) amt
  

-- boundariesSt
--
--   Take the TimeModMark map (*) and convert to a set of BoundaryState data
--   objects, one for each staff.
--
--   (*) This is constructed/filtered from the main Mark3 map foro all Locs
--   and staves
boundariesSt :: Map Loc (Map String [TimeModMark]) -> Map String BoundaryState
boundariesSt mapIn = M.mapWithKey doStaff flippedMap
  where
    -- Filter out just the boundaries/adjust marks, represented as Ba
    -- data. Produce a two level map of Ba data, in which the outer key is the
    -- staff name.
    reduceToBa :: Loc -> Map String [TimeModMark] -> Map String Ba
    reduceToBa loc m = M.mapMaybeWithKey (toBoundaryAdj loc) m
    simplerMap :: Map Loc (Map String Ba)
    simplerMap = M.mapWithKey reduceToBa mapIn
    flippedMap :: Map String (Map Loc Ba)
    flippedMap = flipMap simplerMap

    -- we will fold a BoundaryState data over each staff.
    --
    -- first create an initial state.
    nullState = BoundaryState Nothing False [] []

    -- function that folds Boundary state over one staff of Ba data.
    doStaff :: String -> Map Loc Ba -> BoundaryState
    doStaff stName m = foldl step nullState $ M.toAscList m
      where
        -- 'step'
        --
        -- Do one step in the fold over the list of (Loc,Ba). There are two
        -- cases: either the input Ba is a boundary mark or is an adjust
        -- symbol. 
        step :: BoundaryState -> (Loc,Ba) -> BoundaryState
        -- first case of 'step': there is a boundary mark 
        step bs (loc,BaBound _ mOffset) = case tmsLastBoundary bs of
          -- 
          Nothing ->
            bs { tmsLastBoundary = Just loc
               , tmsGlobAdjFlag = False }
          -- here is where we check if there were no adjusts in the
          -- accumulator
          Just lastLoc -> case tmsAccumAdjust bs of
            [] -> case mOffset of
              -- this is the case there are no adjusts, and the "maybe offset"
              -- on the current boundary mark is Nothing. We reset the
              -- BoundaryState
              Nothing -> bs { tmsLastBoundary = Just loc
                            , tmsGlobAdjFlag = False }
              Just o  -> bs { tmsLastBoundary = Just loc
                            , tmsGlobAdjFlag = False
                            , tmsBoundaries = (OneBoundary (lastLoc,loc)
                                              True [] (Just o))
                                              : tmsBoundaries bs
                            }
            adjs ->
              -- let msg = printf "b(%s,%s)" (showLoc2 lastLoc) (showLoc2 loc)
              -- in msg `trace` 
                    bs { tmsLastBoundary = Just loc
                       , tmsBoundaries = (OneBoundary (lastLoc,loc)
                                          (tmsGlobAdjFlag bs) adjs mOffset)
                         : tmsBoundaries bs
                       , tmsAccumAdjust = []
                       }
        step bs (loc,BaAdj origLoc globFlag amt) = case tmsLastBoundary bs of
          Nothing -> throwMine $ printf ("at %s, there's an adjust " ++
                     "but this is not between two boundaries") (showLoc2 loc)
          Just _ ->
            bs { tmsAccumAdjust = (loc,amt): tmsAccumAdjust bs
               , tmsGlobAdjFlag = globFlag || tmsGlobAdjFlag bs }
                     
          


data Ba = BaBound Loc (Maybe Double)
        | BaAdj Loc Bool Double

        
toBoundaryAdj :: Loc -> String -> [TimeModMark] -> Maybe Ba
toBoundaryAdj loc stName marks = case mapMaybe filt marks of
    [] -> Nothing
    [x] -> Just x
    _   -> throwMine $ printf ("at %s, on staff '%s', there are " ++
                               "multiple boundary/adjust " ++
                               "marks") (showLoc2 loc) stName
  where
    filt (TmmBoundary x y) = Just $ BaBound x y
    filt (TmmAdjust x y z)   = Just $ BaAdj x y z
    filt _ = Nothing


----------------------------------------------------------------------
----------------------------------------------------------------------
----------------------------------------------------------------------
--              code for handling ABSOLUTE WARPS

-- absoluteWarps
--
--   Given a TimeModMark Map (*), produce a list of UnitTimeMod that implement
--   absolute warps.
--
-- (*) This is constructed from the main Mark3 map by filtering for time
--     modification marks only.

absoluteWarps :: Map Loc (Map String [TimeModMark]) -> [UnitTimeMod]
absoluteWarps mIn = concatMap foldStaff $ M.elems x2
  where
    -- 
    x1 :: Map Loc (Map String Aw)
    x1 = M.mapWithKey convertToAMap mIn
    x2 :: Map String (Map Loc Aw)
    x2 = let zz = flipMap x1
         in showMapStringMapLocAw zz `trace` zz
    foldStaff :: Map Loc Aw -> [UnitTimeMod]
    foldStaff m = ms
      where (AbsWarpState _ _ ms) = foldAbsWarp $ M.toAscList m
    convertToAMap :: Loc -> Map String [TimeModMark] -> Map String Aw
    convertToAMap loc = M.mapMaybeWithKey (convertTimeModMark loc)
      where
        convertTimeModMark :: Loc -> String -> [TimeModMark] -> Maybe Aw
        convertTimeModMark atLoc stName ms = case mapMaybe g ms of
          [] -> Nothing
          [a] -> printf "absoluteWarps: did convert Aw %s" (show a)
                   `trace` Just a
          _   -> throwMine $ printf ("error at %s on staff '%s':" ++
                 "multiple 'w' or warp marks at same loc") (showLoc2 atLoc)
                 stName
          where
            g :: TimeModMark -> Maybe Aw
            g (TmmW loc) = Just $ AwW loc
            g (TmmAbsWarp loc ws d) = Just $ AwWarp loc ws d
            g _                     = Nothing


showMapLocMapStringAw :: Map Loc (Map String Aw) -> String
showMapLocMapStringAw m = unlines $ concatMap g $ M.toAscList m
  where
    g :: (Loc,Map String Aw) -> [String]
    g (loc1,m2) = [ showLoc2 loc1 ] ++ map h (M.toAscList m2)
      where
        h :: (String,Aw) -> String
        h (staffN,aw) = printf "staffN: %s, %s" staffN (show aw)
                  

showMapStringMapLocAw :: Map String (Map Loc Aw) -> String
showMapStringMapLocAw m = unlines $ concatMap g $ M.toAscList m
  where
    g :: (String,Map Loc Aw) -> [String]
    g (name,m2) = [ name ] ++ map h (M.toAscList m2)
      where
        h :: (Loc,Aw) -> String
        h (loc1,aw) = printf "%s: %s" (showLoc2 loc1) (show aw)
                  


foldAbsWarp :: [(Loc,Aw)] -> AbsWarpState
foldAbsWarp = foldl step nullState
  where
    nullState = AbsWarpState Nothing Nothing []
    step :: AbsWarpState -> (Loc,Aw) -> AbsWarpState
    step aws (loc1,AwW origLoc) = case awsLastRightWarp aws of
      Just (locLast,amt) ->
        let r = approxRational amt 0.01
            newTm = UnitAbsWarp locLast loc1 r
        in aws { awsLastW = Just loc1
               , awsLastRightWarp = Nothing
               , awsOutput = awsOutput aws ++ [newTm]
               }
      Nothing -> aws { awsLastW = Just loc1 }
    step aws (loc1,AwWarp origLoc side amt) = case side of
      LeftWarp -> case awsLastW aws of
        Just locW ->
          let r = approxRational amt 0.01
              newTm = UnitAbsWarp locW loc1 r
          in aws { awsLastRightWarp = Nothing
                 , awsOutput = awsOutput aws ++ [newTm] }
        Nothing -> throwMine $ printf ("error at %s: the abs warp is left " ++
                                      "sided, but there is not W mark to " ++
                                      "the left.") (showLoc2 loc1)
                                    
      RightWarp -> aws { awsLastW = Nothing
                       , awsLastRightWarp = Just (loc1,amt) }
                     

data AbsWarpState = AbsWarpState
  { awsLastW :: Maybe Loc
  , awsLastRightWarp :: Maybe (Loc,Double)
  , awsOutput :: [UnitTimeMod]
  }

data Aw = AwW Loc
        | AwWarp Loc WarpSide Double
        deriving(Show)

  
                            
                          
       
