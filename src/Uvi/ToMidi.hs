{-# LANGUAGE TupleSections #-}

module Uvi.ToMidi where

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set((\\))
import Debug.Trace
import Text.Printf
import System.Random
import Control.Monad.State
import Data.Map(Map)
import Score.ScoreData
import Instruments
import Instruments.ToUnitTimeMods
import Instruments.ApplyTimeMod
import Instruments.TimeMap
import Instruments.Dynamics
import Midi.MidiData
import Uvi
import Uvi.Util
import Uvi.Show
import Uvi.InstrFuncs
import Util.Exception
import Util.Showable


-- okay so custom data for each AInstr type? then we define

-- 2017-08-05: where did we leave off? was working with so-called PatchSections


toMidi :: Score -> (Int,Maybe Int) -> Double -> Map String AInstr -> StdGen ->
          [ShowItem]
toMidi score msrRange tempoRatio sInstrs gen
  | sInstrsMatchStaves score sInstrs =
      toMidi' score msrRange tempoRatio sInstrs gen


-- PatchSection. 
toMidi' :: Score -> (Int,Maybe Int) -> Double -> Map String AInstr ->
           StdGen -> [ShowItem]
toMidi' score (msrBeg,mMsrEnd) tempoRatio sinstrs gen =
  case runUt toMidiM initSt of
    (Left s,_    ) -> throwMine s
    -- (_     ,final) -> toShorts $ concat (usInitRaws final) ++ concat (usRaws final)
    (Right si,_  ) -> [SingleLine "update toMidi'"]
  where
    initSt = UtState score (msrBeg,msrEnd) tempoRatio sinstrs gen M.empty
             M.empty [] [] [] M.empty
    msrEnd = findEndMsr msrBeg score


-- sInstrsMatchStaves
--
--   Verifies the following:
--
--     - staff names are unique
--
--     - SInstr 
--
--     - there is an SInstr for every staff name
--
--   In the event these things are not true, will raise a sensible error.
--
sInstrsMatchStaves :: Score -> Map String AInstr -> Bool
sInstrsMatchStaves score sInstrs
  | s1 == s2 = True
  | otherwise = throwMine $ printf ("in ToMidi.hs, problem with input SInstr's. A list of " ++
     "SInstr names not present in Score staves: %s. A list of Score staves not present " ++
     "in SInstr names: %s") (show inSOnly) (show inScoreOnly)
  where
    s1 = M.keysSet sInstrs
    s2 = M.keysSet $ scStaves score
    si = S.intersection s1 s2
    inSOnly = s1 \\ si
    inScoreOnly = s2 \\ si

toShorts :: [UtRaw] -> [Short]
toShorts = error "foo"


toMidiM :: Ut ()
toMidiM = do
  makeTimeMaps
  makeLoudnessCurves
  gets usAInstrs >>= mapM_ runSInstr . M.toList
  

runSInstr :: (String,AInstr) -> Ut ()
runSInstr (staffN,sInstr) = do
  score <- gets usScore
  let staff = tmLookup
              (printf "in runAInstr, can't find staff name %s" staffN)
              staffN $ scStaves score
  ps <- makePatchSections staffN
  notes <- concat `liftM` mapM (convPatchSection sInstr) ps
  -- notes <- aiDoNotes ainstr ainstr staff
  modify (\s -> s {usNotes=notes:usNotes s})
  

tmLookup :: Ord k => String -> k -> Map k a -> a
tmLookup msg k m = case M.lookup k m of
  Nothing -> throwMine msg
  Just x  -> x
  

makeTimeMaps :: Ut ()
makeTimeMaps = do
  score <- gets usScore
  tempoRatio <- gets usTempoRatio

  -- compute [UnitTimeMod] by looking at time-altering marks in the score
  let (globTM,staffTM) = computeUnitTimeMods score

  -- compute base time map
  let base = computeBaseTimeMap score tempoRatio globTM

  -- compute 'staffTMs', a Map of staff name to staff time map
  let doStaff :: String -> (String,RelTimeMap)
      doStaff staffName = case M.lookup staffName staffTM of
        Nothing   -> (staffName,base)
        Just mods -> (staffName,foldl (applyTimeMod (scTimeSigs score)) base mods)
      staffTMs = M.map toAbsolute $
                 M.fromList $ map doStaff (M.keys $ scStaves score)

  -- Update time maps and list of time mods in the monad state.
  modify (\s -> s {usTimeMaps = staffTMs})
  modify (\s -> s {usTimeMods = staffTM })


makeLoudnessCurves :: Ut ()
makeLoudnessCurves = do
  score <- gets usScore
  atms  <- gets usTimeMaps
  -- we need to store curves in staff stateB
  let loudnessCurves = hpDynCurves atms score
      expandToVoices :: Curve -> Map Int Curve
      expandToVoices c = M.fromList $ map (,c) [1..4]
  modify (\s -> s {usLoudness = M.map expandToVoices loudnessCurves})


-- what could be goal? play any notes at all? how about listing of patch sections?


-- what is involved in translating a score to midi?
--
-- staff time maps: for each staff,
--
--   construct base time map from global time marks (including time sigs and
--   global tempo indiciations)
--
--   construct staff-specific time maps using staff time marks
--
--   this could be simplified by computing each staff's time map from a single
--   list, but not worth it
--
-- staff loudness curve
--
--   construct base curve from dynamics and hairpins
--
--   account for staff mark CresDescr
--   
-- for each chord
--
--   figure out dest for that chord
--
--     pick CInstr for that chord (will be the same for every note) and from
--     that get dest
--
--
--   figure out nominal begin and end times for each note
--
--     begin based on Loc, and end on true end Loc, looking up both Locs in
--     TimeMap to get time as Double
--
--   figure out note velocity: the method depends on the CInstr
--
--     1. possibly just a constant, such as 64
--
--     2. possibly dynamics at that point in the staff, lookup up in dyn map
--        (maybe at some point we'll have different dyn maps for different
--        voices)
--
--          - still need some way to translate dynamics (loudness) to actual
--            MIDI velocity; this would probably be unique for each patch
--
--     3. possibly desired accent hardness which could be based on how short
--        the note is or its dynamic level
--
--     should a CInstr include a field which is a function for determining
--     velocity? as opposed to what? custom kind of data? 
--
-- for each staff
--
--   construct mod curve: possibly consider dynamics and local additional curves
--
--   possibly make dynamics into a control curve 
--
--     


data CInstr = CiIrTrumpetOrd (Int,Int) -- 
            | CiPiano 
