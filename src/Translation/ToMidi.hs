{-# LANGUAGE TupleSections #-}

module Translation.ToMidi where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Function
import Control.Arrow
import Data.Array
import Data.Maybe
import Debug.Trace
import Text.Printf
import Data.Either
import System.Random
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens
import Data.Map(Map)
import Translation
import Translation.InstrUtils
import Midi.MidiData
import Score.ScoreData
import Score.FindEnd
import Translation.TimeMap
import Translation.ToUnitTimeMods2
import qualified Translation.ApplyTimeMod2 as ATM2
import Translation.Dynamics
import Util.Exception
import Util.Showable
import Common.CommonUtil
import Common
import Translation.Curves
import Translation.GenericShape
import Translation.Splice2

-- we need to represent unit time mods so that they can be sorted into
-- order of application and whether global or staff-only
 
analyzePcRepetition :: Tr String
analyzePcRepetition = do
  score <- gets (view score) :: Tr Score
  let loc1 = Loc 19 1
      loc2 = Loc 22 1
      locPred l = loc1 <= l && l <= loc2
      getChords :: Map Loc (Map Int Chord) -> [Map Int Chord]
      getChords m = M.elems $ M.filterWithKey (\k _ -> locPred k) m
      f1 :: [Staff]
      f1 = M.elems $ scStaves score
      f2 :: [Map Loc (Map Int Chord)]
      f2 = map stChords f1
      f3 :: [Map Int Chord]
      f3 = concatMap getChords f2 
      allChords :: x [Chord]
      allChords = concatMap M.elems f3
      fc :: Chord -> [Int]
      fc (Chord _ _ (NSingles m) _) = map (midiPitch . nPitch) $ M.elems m
      allPitches = concatMap fc allChords
      countPitch :: Int -> [Int] -> Int
      countPitch pc ps = length $ filter ((==pc) . flip mod 12) ps
      toString :: Int -> [Int] -> String
      toString i ps = (concat $ replicate c (s ++ " ")) ++ "\n"
        where
          c = countPitch i ps
          s = [ "C ", "C#", "D ", "D#", "E ", "F ", "F#"
              , "G ", "G#", "A ", "A#", "B "] !! (i `mod` 12)
  return $ "\n" ++ concatMap (flip toString allPitches) [0..11]
      

-- toMidi
--
--   - initialize
toMidi :: (Int,Maybe Int) -> [String] -> Tr ([SNote],[Short])
toMidi (begMsr,mEndMsr) splicePts = do

  -- analyze repetitions of pitch classes
  msg <- analyzePcRepetition
  
  -- find end measure
  score <- (printf "") `trace` gets (view score)
  let endMsr = case mEndMsr of
        Just x  -> x
        Nothing -> findEndMsr begMsr splicePts score

  -- make time maps
  makeTimeMaps

  -- make loudness curves
  makeLoudnessCurvesTr2

  -- make control curves
  -- makeControlCurves
  
  -- ***Run*** each meta instrument by calling 'callMetaRun'
  metas <- M.toList `liftM` (gets $ view metas)
  mapM_ (callMetaRun begMsr endMsr) metas 

  -- this will normalize notes to start 0.1 seconds into the track
  -- mSpliceBegEnd <- getSpliceTimes maybeSplicePoint
  currNotes <- concat `liftM` gets (view notesOut)
  currRaws  <- concat `liftM` gets (view rawsOut)
  (notes2,raws2) <- spliceEverything splicePts (currNotes,currRaws)
  let (newNotes,newRaws) = normalize 0.1 (notes2,raws2)
  initRaws  <- concat `liftM` gets (view initRaws)

  -- convert notes, raws, and init raws into shorts
  -- 
  return (newNotes,convertToShorts newNotes newRaws initRaws)


callMetaRun :: Int -> Int -> (String,MetaInstr) -> Tr ()
callMetaRun msrB msrE (staffN,instr) = do
  metaPre <- prepareMetaRun msrB msrE instr
  case metaPre of
    Nothing -> return ()
    Just p  -> case instr of
      MetaInstr _ _ dat _ run _ -> run instr dat p


prepareMetaRun :: Int -> Int ->  MetaInstr -> Tr (Maybe MetaPrepared)
prepareMetaRun msrB msrE instr = do
  let oneStaff staffN = do
        sns <- allStaffSNote staffN (iSplitTrillFlag instr) msrB msrE
        let locB = minimumToList $ map snLoc sns
            locE = maximumToList $ map snEnd2 sns
        return ((staffN,sns),(locB,locE))
  out <- mapM oneStaff $ iStaffNs instr ::
           Tr [((String,[SNote]),([Loc],[Loc]))]
     
  let sNoteMap = M.fromList $ map fst out
      bounds = mconcat $ map snd out
  case maybeBounds bounds of
        Nothing -> ("no notes; skipping " ++ (iName instr)) `trace`
                   return Nothing
        Just bs -> return $ Just $ MetaPrepared sNoteMap bs


maybeBounds :: ([Loc],[Loc]) -> Maybe (Loc,Loc)
maybeBounds (bsB,bsE) = case (bsB,bsE) of
  ([],_)  -> Nothing
  (_,[])  -> Nothing
  (xs,ys) -> Just (minimum xs,maximum ys)


minimumToList :: Ord a => [a] -> [a]
minimumToList [] = []
minimumToList xs = [minimum xs]


maximumToList :: Ord a => [a] -> [a]
maximumToList [] = []
maximumToList xs = [maximum xs]


tmLookup :: Ord k => k -> Map k a -> a
tmLookup k m = case M.lookup k m of {Just x -> x}


normalize :: Double -> ([SNote],[TrRaw]) -> ([SNote],[TrRaw])
normalize delay (notesIn,rawsIn) = out
  where
      m1 :: Double
      m1 = case notesIn of
             [] -> throwMine "in normalize, no notes present"
             xs -> minimum $ map getTOn xs
      m2 :: Double
      m2 = case rawsIn of
             [] -> m1
             rs -> min m1 (minimum $ map trTime rs)
      offsetRaw d r = r {trTime=trTime r+d}
      out = ( map (offsetOne (-m2+delay)) notesIn
            , map (offsetRaw (-m2+delay)) rawsIn )

  
offsetOne :: Double -> SNote -> SNote
offsetOne d note@SNote{snOnOff = oo@((_,(t1,t2)):_)} = note {snOnOff = new}
  where
    new = (printf "offset %.3f" d,(t1+d,t2+d)):oo


-- gsToUtms
--
--   Find all GenericShape marks on each staff. Compute, from that,
--   a list of GsCombine. Call that list C.
--
--   For each MetaInstr M:
--
--     Look up the GsFunc associated with that MetaInstr.
--
--     Filter out from C all the GsCombined that belong to a staff that
--     belongs to M. Call that filtered list F.
--
--     For each GsCombined in F, pass it to M's GsFunc. Give error if
--     the result is an empty list. Otherwise add the result to a growing
--     list of Utms.
--
gsToUtms :: Tr [Utm]
gsToUtms = do
  comIn <- computeGsCombined
  timeSigs <- scTimeSigs `liftM` gets (view score) 
  --  'f'
  --  Input: MetaInstr
  --  Algorithm: check staves that are part of this meta. Example all input
  --             GsCombined that belong to any of those staves.
  --  Output: UnitTimeMod resulting from conversion of any such GsCombined
  let f :: MetaInstr -> [Utm]
      f instr = concatMap g comIn
        where
          staffNs = iStaffNs instr
          -- 'g' : take GsCombined, see if its staff matches any of the
          --  staves in meta 'instr', and if so use the provided functions
          --  to process it
          g :: GsCombined -> [Utm]
          g c | gcStaffN c `elem` staffNs = case iShapeFn instr timeSigs c of
                  [] -> throwMine $ printf ("Err: for generic shape at %s " ++
                        "on staff '%s', the meta '%s' didn't convert " ++
                        "it to any unit time mods.") (showLoc2 $ gcLoc c)
                        (gcStaffN c) (iName instr)
                  us -> us 
                  
              | otherwise = []
  (concatMap f . M.elems) `liftM` gets (view metas)

{-
makeLoudnessCurves :: Tr ()
makeLoudnessCurves = do
  score <- gets (view score)
  atms  <- gets (view timeMaps)
  -- we need to store curves in staff stateB
  --  loudnessCurves :: 
  let loudnessCurves = hpDynCurves atms score
      expandToVoices :: Curve -> Map Int Curve
      expandToVoices c = M.fromList $ map (,c) [1..8]
  modify (set loudnessMaps $ M.map expandToVoices loudnessCurves) 
-}

makeLoudnessCurvesTr2 :: Tr ()
makeLoudnessCurvesTr2 = do
  score <- gets (view score)
  atms  <- gets (view timeMaps)
  --  loudnessCurves :: Map String [Curve]
  let loudnessCurves = hpDynCurves atms score
      d1 :: Map String Staff
      d1 = scStaves score
      d2 :: Map String (Map Int Curve)
      d2 = makeLoudnessCurvesNotTr d1 loudnessCurves
  modify (set loudnessMaps d2)

-- MUSESCORE_UPGRADE
makeLoudnessCurvesNotTr :: Map String Staff -> Map String [Curve] -> Map String (Map Int Curve)
-- makeLoudnessCurvesNotTr staffToVoices = M.map (expandCurvesToVoices staffToVoices)
makeLoudnessCurvesNotTr st cs
  | M.keys st == M.keys cs = M.intersectionWith expandCurvesToVoices st cs


-- expandCurvesToVoices - MUSESCORE_UPGRADE
--   given [Curve], one Curve for each Sib/MuseScore staff, expand each Curve
--   to all the voices on that staff
-- params:
--   Map Int [Int] - map of staff number to all voices on that staff
expandCurvesToVoices :: Staff -> [Curve] -> Map Int Curve
expandCurvesToVoices staff curves = M.unions $ map f2 d1
  where
    staffToVoices = stStaffToVoice staff
    -- d1: list of (<staff number>, <curve>) tuples
    d1 :: [(Int,Curve)]
    d1 = zip [1..] curves
    -- f1: given a Curve and a list of voices, generate map of voice number to that
    --     same curve as every elem
    f1 :: Curve -> [Int] -> Map Int Curve
    f1 c vs = M.fromList (zip vs (repeat c))
    -- f2: given a tuple of staff number and associated Curve, expand to map 
    --    of voice number to that same curve as every elem
    f2 :: (Int,Curve) -> Map Int Curve
    f2 (staffN,curve) = case M.lookup staffN staffToVoices of
      Just vs -> f1 curve vs
{-
makeGenericShapes :: 
  let gsMarks = computeGenericShapeMarks marks
-}


makeTimeMaps :: Tr ()
makeTimeMaps = do
  score <- gets (view score)
  let staffNames = M.keys $ scStaves score
      timeSigs = scTimeSigs score
      marks = locsRoundToSlice timeSigs $ scMarks score
      
  -- 'base' time map: the one considering only SetTempo and ramp's
  let baseTimeMap = computeBaseTimeMap score 1

  -- Now compute unit time mods (Utms)
  let tmsMarks = computeDirectUtms marks
  tmsShapes <- gsToUtms
  let tms = L.sortBy (compare `on` utmRank) $ tmsMarks ++ tmsShapes

  -- now compute the UnitTimeMods
  let tmsGlob = filter ((==Nothing) . utmStaffN) tms
      tm2 = foldl (ATM2.applyTimeMod timeSigs) baseTimeMap tmsGlob
  
  tm3 <- randomizeTimeMap tm2
  let doStaffTimeMap :: String -> RelTimeMap -> RelTimeMap
      doStaffTimeMap staffN tmIn = tmOut 
        where
          tmOut = foldl (ATM2.applyTimeMod timeSigs) tmIn xs
          xs = filter ((==(Just staffN)) . utmStaffN) tms
      unalteredMaps = M.fromList $ map (id &&& const tm3) staffNames
      finalRelMaps = M.mapWithKey doStaffTimeMap unalteredMaps
      absMaps = M.map toAbsolute finalRelMaps
  -- modify (\s -> s { tsTimeMaps = absMaps
  --                 , tsRelTimeMaps = finalRelMaps })
  modify (set timeMaps absMaps . set relTimeMaps finalRelMaps)


-- convertToshorts
--
--   Given notes, and two forms of raws, convert all to shorts and
--   concatenate. I don't know why this is its own routine.
--
convertToShorts :: [SNote] -> [TrRaw] -> [TrRaw] -> [Short] 
convertToShorts notes raws initRaws = out
  where
    notesAsShort = concatMap noteToShort notes
    rawsAsShort  =  map rawToShort raws
    initRawsAsShort = map rawToShort initRaws 
    out = case notesAsShort of
      [] -> throwMine "no notes, in convertToShorts in ToMidi2.hs"
      _  -> notesAsShort ++ rawsAsShort ++ initRawsAsShort


noteToShort :: SNote -> [Short]
noteToShort SNote { snOnOff = (_,(t1,t2)):_
                   , snDest = (stream,chan)
                   , snPitch = pit
                   , snVel   = vel
                   , snMods  = mods } =
    [Short t1 stream statusOn pit vel, Short t2 stream statusOff pit 64] ++
    modsOut
  where
    statusOn  = 0x90+chan-1
    statusOff = 0x80+chan-1
    modsOut = concatMap (toShortMod t1 t2 (stream,chan)) mods


toShortMod :: Double -> Double -> (Int,Int) -> Modif -> [Short]
toShortMod t1 t2 (stream,chan) (ModifKs timing key) =
    [ Short tOn  stream (0x90+chan-1) key 64
    , Short tOff stream (0x80+chan-1) key 64 ]
  where
    tOn = case timing of
      Left d -> t1+d
    tOff = tOn + 0.05
toShortMod t1 t2 (stream,chan) (ModifCtrl timing ctrlNum value) =
    [ Short tOn  stream (0xB0+chan-1) ctrlNum value]
  where
    tOn = case timing of
      Left d -> t1+d



rawToShort :: TrRaw -> Short
rawToShort (TrRaw _ t (stream,chan) status data1 data2)
  = Short t stream (status+chan-1) data1 data2

{-
findNominalTimes :: OnOff -> (Double,Double)
findNominalTimes (OnOff list) = snd x
  where x = case L.find ((== "nominal") . fst) list of {Just y -> y}
-}

debugDumpShowItem :: String -> ShowItem -> IO ()
debugDumpShowItem filename content = do
  let s1 = showiToString content
  putStrLn ("writing " ++ filename) >> writeFile filename s1


