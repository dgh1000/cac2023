{-# LANGUAGE TupleSections, TypeSynonymInstances, FlexibleInstances #-}

module Instruments.Synth3 where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Debug.Trace
import Text.Printf
import Data.Map.Strict(Map)
import Data.List(sortBy)
import Data.Maybe
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.Function
import Instruments.InstrumentsData
import Instruments.InstrUtils
import Score.ScoreData
import Common
import Common.CommonUtil
import Util.Exception
import Util.Showable
import Util.Map


-- <maybe last artic> <open brackets>
data FoldStateStaff = FoldStateStaff (Maybe (Loc,String))


data FoldState = FoldState (Map String FoldStateStaff)


data MapAccumOut = MaoArtic String Loc String
                 | MaoBracket String Loc Loc String







----------------------------------------------------------------------

instance ShowItemClass SynthArticBracket where
  showI = SingleLine . show


instance ShowItemClass MapAccumOut where
  showI (MaoArtic staffN loc s) = SingleLine $ printf "MaoArtic %s %s %s"
    staffN (showLoc2 loc) s
  showI (MaoBracket staffN loc1 loc2 s) = SingleLine $
    printf "MaoBracket %s %s %s %s" staffN (showLoc2 loc1) (showLoc2 loc2) s


instance ShortString Loc where
  showSh = showLoc2


instance ShortString String where
  showSh = id

instance ShowItemClass FoldStateStaff where
  showI (FoldStateStaff mB) = SingleLine $ printf "mBrack: %s" (show mB)


----------------------------------------------------------------------




staveNames = ["Piano-staff1", "Piano-staff2"]


makeSynth :: MetaInstr
makeSynth = MetaInstr "synth" staveNames s runSynth
  where
    s = Synth M.empty M.empty M.empty M.empty
      (SynthState M.empty [] M.empty)
 

mkTestNote staffN = SNote "" [] staffN (Loc 1 1) (Loc 1 2) 1
                (Chord (Loc 1 2) S.empty (NSingles M.empty))
                (Note (Pitch 60 0 0 0) False (Loc 1 2) NormalHead)
                [("nominal",(0,1))] 0 (0,1)
                60 60 64 [] 0 0.001 NoTrill


-- we translate LowLevelNote to possibly many lower level notes, which can
-- have different dests, can have curves


-- TrNote can have multiple dests. it might be better to do this through
-- SuperLowLevelNote.

-- jobs of synth:
--
-- any given note on the score could be sent to various destinations, with
-- various parameters, based on
--
--   patch or articulation marks in the score
--
--     this would choose the destination, as well as the method of converting
--     to velocity and whether there would be a mod wheel control of velocity
--
--   sections marked off with [name    ]
--
--     this could choose patch and articulation as well as construct shapes
--     in controls or mod wheel.
--
--     they would be specific to the composition. so they would be programmed
--     in the script file for that composition.
--
--     
--
-- should we just say right now that loudness is always controlled by mod
-- wheel? keep it consistent
--
-- an algorithm could split notes into different destinations. we would like
-- to write code simple functions.l 
--
-- what state will we preserve as we translate notes?
--
--   we have already fixed patches at destinations. so I guess it's the
--   artic. could that change with every note? the thing is we don't want to
--   interrupt notes in progress, need to preserve last artic settings. some
--   settings will modulate notes individually. 

runSynth :: MetaInstr -> Synth -> Int -> Int -> Tr MetaInstr
runSynth instr synth msrBeg msrEnd = do
  -- we need low level notes for *all* staves
  notes <- runSynth2 instr msrBeg msrEnd
  marks    <- scMarksByStaff `liftM` gets tsScore
  -- xs: list of data in which each element is tagged with a staff name
  -- and a loc. data is a sum of artic and bracket data
  let xs :: [StaffLocTagged SynthArticBracket]
      -- convert marks to the type of 'xs'
      xs = L.sort $ toSltSL $ M.map (lMapMaybe toSab) marks
      st = FoldState $ M.fromList $ map (,FoldStateStaff Nothing) $
           iStaffNs instr
      ys :: [MapAccumOut]
      ys = catMaybes $ snd $ L.mapAccumL foldSab st xs
      (mapArtic,mapBrack) = maoToMaps ys

  notes2 <- doNotes mapArtic mapBrack notes
  modify (\s ->
            s {tsDebugOut =
               [showiToString $ Component "" False (map showI ys)]})
  
  -- include test notes
  includeNotes . map mkTestNote $ iStaffNs instr
  
  return instr
  

toSab :: Mark3 -> Maybe SynthArticBracket
toSab (BracketL3 s) = Just $ SabBrackL s
toSab (BracketR3 s) = Just $ SabBrackR s
toSab (Artic3    s) = Just $ SabArtic  s
toSab _             = Nothing


-- runSynth1
--
--   First part of runSynth algorithm. Constructs a map of SNotes, organized
--   by Loc and staff name.
--
--   Do they need to be organized by 
runSynth1 :: MetaInstr -> Int -> Int -> Tr (MapStaffLoc [SNote])
runSynth1 instr msrBeg msrEnd = do
  let staffNs = iStaffNs instr
      mkSNotes :: String -> Tr (String,Map Loc [SNote])
      mkSNotes staffN = do
        notes <- allStaffSNote staffN True msrBeg msrEnd 
        return $ ( staffN
                 , M.fromListWith (++) $ map (\n -> (snLoc n,[n])) notes)
  (MapStaffLoc . M.fromList) `liftM` mapM mkSNotes staffNs


runSynth2 :: MetaInstr -> Int -> Int -> Tr [SNote]
runSynth2 instr msrBeg msrEnd = do
  let staffNs = iStaffNs instr
      mkSNotes :: String -> Tr [SNote]
      mkSNotes staffN = allStaffSNote staffN True msrBeg msrEnd
  concat `liftM` mapM mkSNotes staffNs


maoToTuple (MaoArtic staffN loc articName) =
   ( [(staffN,(loc,articName))], [                                ] )
maoToTuple (MaoBracket staffN loc1 loc2 brackName) =
   ( [                        ], [(staffN,(loc1,(loc2,brackName)))] )


maoToMaps :: [MapAccumOut] -> (MapStaffLoc String, MapStaffLoc (Loc,String))
maoToMaps ms = (y,z)
  where
    xs :: ([(String,(Loc,String))],[(String,(Loc,(Loc,String)))])
    xs = mconcat $ map maoToTuple ms
    y :: MapStaffLoc String
    y = mslFromTuples $ fst xs
    z = mslFromTuples $ snd xs




-- foldSab
--
-- Do one step in accumulating fold over articulation and bracket markings.
--
foldSab :: FoldState -> StaffLocTagged SynthArticBracket ->
           (FoldState,Maybe MapAccumOut)

-- foldSab for SabArtic
foldSab (FoldState fs) (StaffLocTagged staffN loc (SabArtic s)) =
    (FoldState outState,Just $ MaoArtic staffN loc s)
  where
    outState = case M.lookup staffN fs of
      Just (FoldStateStaff Nothing) -> fs
      otherwise -> throwMine $ printf ("problem at artic mark '%s' " ++
                   "on staff '%s' at %s. Maybe inside brackets?" )
                   s staffN (showLoc2 loc)


-- foldSab for SabBrackL
foldSab (FoldState fs) (StaffLocTagged staffN loc (SabBrackL s)) =
    (FoldState outState,Nothing)
  where
    openMatchingName (FoldStateStaff Nothing) = False
    openMatchingName (FoldStateStaff (Just (_,openName))) = openName == s
    outState | all (not . openMatchingName) $ M.elems fs = 
                 M.insert staffN (FoldStateStaff $ Just (loc,s)) fs
             | otherwise = throwMine $ printf ("problem with left " ++
                           "bracket on staff '%s' at loc %s, named '%s':" ++
                           " another bracket with this name on some staff " ++
                           "is already open") staffN (showLoc2 loc) s


-- foldSab for SabBrackR
foldSab (FoldState fs) (StaffLocTagged staffN loc (SabBrackR s)) =
    (FoldState outState,Just $ MaoBracket staffN openLoc loc s)
  where
    err = throwMine $ printf ("problem at right brack mark '%s' on " ++
          "staff '%s' at %s: there is no open bracket open, or " ++
          "one open of a different name") s staffN (showLoc2 loc)
    (outState,openLoc) = case M.lookup staffN fs of
      Just (FoldStateStaff (Just (openLoc,openName)))
        | openName == s ->
            (M.insert staffN (FoldStateStaff Nothing) fs,openLoc)
        | otherwise -> err
      otherwise -> err


-- doNotes
--
--   Given a list of SNotes that are filled with nominal
--   (non-instrument-specific) data like pitch and time, produce a list of
--   SNote that are customized to operate the synthesizer.
--
--   Ancillary data include a list of artic marks and bracket marks.
--
doNotes :: Synth -> MapStaffLoc String -> MapStaffLoc (Loc,String) ->
           [SNote] -> Tr [SNote]
doNotes syn (MapStaffLoc mapArtic) (MapStaffLoc mapBrack) notes = do
  -- We define 'lookupStLE', a way to help up look up either an artic mark
  -- or a bracket that applies to a note at input loc 'loc'
  let lookupStLE staffN loc m = M.lookupLE loc (sLookup staffN m)
      doNote :: SNote -> Tr [SNote]
      doNote s = case (mBrack,mArtic) of
            (Just b,_) -> doNoteBrack syn b s
            (_,Just a) -> error "mnmnmn534"
        where
          -- find loc of this note. look up whether in bracket region
          -- or artic
          mBrack :: Maybe String
          mBrack = case lookupStLE (snStaffName s) (snLoc s) mapBrack of
            Nothing -> Nothing
            Just (loc2,name) | snLoc s < loc2 -> Just name
                             | otherwise      -> Nothing
          mArtic = lookupStLE (snStaffName s) (snLoc s) mapArtic
          
  return $ concatMap doNote notes


doNoteBrack :: Synth -> String -> SNote -> Tr [SNote]
doNoteBrack synth brackName s = oneFn s
  where
    (FuncBracket oneFn _) = sLookup brackName $ syBrackFns synth


sLookup k m = case M.lookup k m of
  Just x -> x
