
module Instruments.Synth where

import qualified Data.Map.Strict as M
import Text.Printf
import Data.Map.Strict(Map)
import Data.List(sortBy)
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Data.Function
import Instruments.InstrumentsData
import Instruments.InstrUtils
import Score.ScoreData
import Common
import Common.CommonUtil
import Util.Exception


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
  let staffNs = iStaffNs instr
      f staffN = now_allStaffSNote staffN True msrBeg msrEnd
  allNs <- (sortBy (compare `on` snLoc) . concat) `liftM` mapM f staffNs
  let initState = SynthState M.empty [] M.empty
  SynthState _ notes raws <- foldM (doNote synth) initState allNs
  includeNotes notes
  includeRaws . concat $ M.elems raws 
  return instr


-- data BrData = BrData (Maybe String) (Maybe String)

data BrArticData = BadArtic String
                 | BadOpen  String
                 | BadClose String


lookupBrAtLoc :: Loc -> [Mark3] -> (Maybe String,Maybe String)
lookupBrAtLoc loc ms = case (mapMaybe isBracketL ms,mapMaybe isBracketR ms) of
  ([] ,[] ) -> (Nothing,Nothing)
  ([x],[] ) -> (Just x ,Nothing)
  ([] ,[x]) -> (Nothing, Just x)
  _         -> throwMine $ printf ("In Synth.hs, there are multiple left" ++
               " brackets or multiple right brackets at %s") (showLoc2 loc)
                 

lookupBrArticAtLoc :: Loc -> Map Loc [Mark3] -> [BrArticData]
lookupBrArticAtLoc loc m = catMaybes [ BadClose <$> mClose
                                     , BadOpen  <$> mOpen 
                                     , BadArtic <$> mArtic ]
  where
    (mArtic,(mOpen,mClose)) = case M.lookup loc m of
      Nothing -> (Nothing,(Nothing,Nothing))
      Just ms -> ( g ms
                 , lookupBrAtLoc loc ms )
    g :: [Mark3] -> Maybe String
    g ms = case mapMaybe isArtic ms of
      []  -> Nothing
      [x] -> Just x
      _   -> throwMine $ printf ("in Synth.hs, more than one artic " ++
                                 "mark at loc %s") (showLoc2 loc)
    

{-

doNote :: SynthState -> SNote -> Tr SynthState
doNote ss note = do
  let staffN = snStaffName note
      loc    = snLoc note
  staffMarks <- (sLookup staffN . scMarksByStaff) `liftM` gets tsScore
  let br = lookupBrAtLoc loc staffMarks
      mArt = M.lookup loc staffMarks >>= g
      lastSss = sLookup staffN $ ssStaffState ss
      newSss = case (mArt,br) of
        (Just artName,BrData Nothing Nothing)
           | isSssOpenBracket lastSss ->
               throwMine ("9po4 "++ showLoc2 loc)
           | otherwise                   -> SssArtic artName
        (Nothing,BrData (Just o) Nothing)
           | isSssOpenBracket lastSss ->
               throwMine ("n289 "++ showLoc2 loc)
           | otherwise                   -> SssOpenBracket loc o
        (Nothing,BrData Nothing  (Just c))
          | isSssOpenBracket lastSss -> 
              if sssOpen lastSss == c
                then SssNone
                else throwMine ("knv76 "++showLoc2 loc)
          | otherwise -> throwMine ("wtz25 "++ showLoc2 loc)
        (Nothing,BrData (Just o) (Just c)) 
          | isSssOpenBracket lastSss ->
            if c == sssOpen lastSss
              then SssOpenBracket loc o
              else throwMine ("zzz "++showLoc2 loc)
          | otherwise -> throwMine ("mqqq "++showLoc2 loc)
        otherwise -> throwMine ("iqoqp2" ++ showLoc2 loc)
      -- now figure out if we have to initialize any articulation controls
      -- case newSSS of
      --   SssLastArtic -> 
  
  -- well in general we have to find artic or bracket that is operative at
  -- this point in the composition. and whether that changes. so first thing
  -- is to find if there is an artic mark 
  --
  -- case br of
  error "foo"

-}

isSssOpenBracket (SssOpenBracket _ _) = True
isSssOpenBracket _                    = False

sssOpen (SssOpenBracket _ s) = s

-- what's the simplest language I can use to describe this?
--
--   when we are neutral and encounter an artic, set up artic. interpret all
--   future notes as that artic. (error to encounter an artic when not neutrol)
--
--   if neutral or in artic and we encounter an open bracket, record its
--   beginning and fact we are in the bracket
--
--   if we encounter a closed bracket
--
--   

doNote :: Synth -> SynthState -> SNote -> Tr SynthState
doNote sy ssIn note = do
  let staffN = snStaffName note
      loc    = snLoc note
  staffMarks <- (sLookup staffN . scMarksByStaff) `liftM` gets tsScore
  let bd = lookupBrArticAtLoc loc staffMarks
      step :: SynthState -> BrArticData -> Tr SynthState
      step ssStep badIn = do
        let curr = sLookup staffN $ ssStaffState ssIn
        case curr of
          -- this is case that in 'none' state we encounter an artic for
          -- the first time
          SssNone -> case badIn of
            BadArtic s -> newState_initArtic sy s ssStep
            BadOpen  s -> newState_openBracket staffN s loc ssStep
          SssOpenBracket openLoc openName -> case badIn of
            BadClose s | s == openName ->
                           newState_closeBracket sy staffN openLoc loc ssStep
          SssArtic s -> case badIn of
            BadArtic s -> newState_initArtic sy s ssStep
            BadOpen  s -> newState_openBracket staffN s loc ssStep
  newSS <- foldM step ssIn bd
  -- lookup translation function for this note
  let f = case sLookup staffN $ ssStaffState newSS of
        SssOpenBracket _ bracketName ->
          case M.lookup bracketName $ syBrackFns sy of
            Just (FuncBracket f _) -> f
        SssArtic articName ->
          case M.lookup articName $ syArticFns sy of
            Just (FuncArtic   f _) -> f
  outNotes <- f note
  return $ newSS { ssNotes = outNotes ++ ssNotes newSS }


newState_openBracket :: String -> String -> Loc -> SynthState ->
                        Tr SynthState
newState_openBracket staffN brackName loc ssIn = do
  let m = ssStaffState ssIn
  -- make sure no open bracket of same type on any other staff
  return $
    ssIn { ssStaffState = M.insert staffN (SssOpenBracket loc brackName) m }


newState_closeBracket :: Synth -> String -> Loc -> Loc -> SynthState ->
                         Tr SynthState
newState_closeBracket sy staffN loc1 loc2 ssIn = do
  let FuncBracket _ f = sLookup staffN $ syBrackFns sy
  notes <- f loc1 loc2
  return $ ssIn {ssNotes = notes ++ ssNotes ssIn}


newState_initArtic :: Synth -> String -> SynthState -> Tr SynthState
newState_initArtic sy artic ssIn = error "foo"

{-

-- checkCase1
--
--   Looking at 'loc' on 'staffN', this is the case that an artic mark is
--   present, while no brackets are present.
--
--   The last state must be as follows: SssNone or SssLastArtic
checkCase1 :: SynthStaffState -> SynthStaffState -> SynthStaffState
checkCase1 SssNone passThroughSss = passThroughSss
checkCase1 (SssLastArtic _) = passThroughSss


-- handleCase2
--
--   Looking at 'loc' on 'staffN', this is case that a left bracket (but no
--   artic or right bracket) is present.
--
handleCase2 :: Loc -> SynthStaffState -> String -> SynthStaffState
handleCase2 _ SssNone s = SssOpenBracket s
handleCase2 loc (SssOpenBracket s1) s2 = throwMine $ printf ("in Synth.hs, "++
                                         "found a left bracket '%s' when " ++
                                         "there " ++
                                         "was already an open bracket '%s', "++
                                         "at %s") s2 s1 (showLoc2 loc)
handleCase2 loc (SssLastArtic _) s2 = SssOpenBracket s2


handleCase3 :: SynthStaffState -> String -> SynthStaffState 
handleCase3 = error "foo"


handleCase4 :: SynthStaffState -> String -> String -> SynthStaffState
handleCase4 = error "foo"
-}


sLookup k m = case M.lookup k m of
  Just x -> x
