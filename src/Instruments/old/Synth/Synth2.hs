
module Instruments.Synth2 where

import qualified Data.List as L
import qualified Data.Set as S
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
import Util.Showable
import Util.Map

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
  notesMap <- runSynth1 instr msrBeg msrEnd
  atLocMap <- toAtLocStaff notesMap

  -- include debugging information
  -- modify (\s -> s {tsDebugOut = [showIString $ AtLocMap atLocMap]})

  -- include test notes
  includeNotes . map mkTestNote $ iStaffNs instr
  
  return instr
  
  {-
  -- allNs <- (sortBy (compare `on` snLoc) . concat) `liftM` mapM f staffNs
  let initState = SynthState M.empty [] M.empty
  SynthState _ notes raws <- foldM (doNote synth) initState allNs
  includeNotes notes
  includeRaws . concat $ M.elems raws 
  return instr
  -}


-- runSynth1
--
--   First part of runSynth algorithm. Constructs a map of SNotes, organized
--   by Loc and staff name.
--
runSynth1 :: MetaInstr -> Int -> Int -> Tr (Map String (Map Loc [SNote])) 
runSynth1 instr msrBeg msrEnd = do
  let staffNs = iStaffNs instr
      mkSNotes :: String -> Tr (String,Map Loc [SNote])
      mkSNotes staffN = do
        notes <- allStaffSNote staffN True msrBeg msrEnd 
        return $ ( staffN
                 , M.fromListWith (++) $ map (\n -> (snLoc n,[n])) notes)
  M.fromList `liftM` mapM mkSNotes staffNs


toAtLocStaff :: Map String (Map Loc [SNote]) ->
                Tr (Map String (Map Loc AtLocStaff))
toAtLocStaff notesMap = M.fromList `liftM` (mapM xx2 $ M.toAscList notesMap)
   where
     xx2 :: (String,Map Loc [SNote]) -> Tr (String,Map Loc AtLocStaff)
     xx2 (staffN,notesMap) = do
       let xx3 :: (Loc,[SNote]) -> Tr (Loc,AtLocStaff)
           xx3 (loc,notes) = do
             marks <- scMarksByStaff `liftM` gets tsScore
             let (a,l,r) = case M.lookup loc $ sLookup staffN marks of
                   Nothing -> (Nothing,Nothing,Nothing)
                   Just ms -> oneAtLocStaff staffN loc ms
             return $ (loc,AtLocStaff a l r)
       m2 <- M.fromList `liftM` (mapM xx3 $ M.toAscList notesMap)
       return (staffN,m2)


type Accum1 = Maybe (Loc,String)


type Accum2 = Map String (Maybe (Loc,String))


type Bd = (String,(Loc,Loc))


type BdAll = Map String [Bd]


data BrackType = BrackL String | BrackR String


accum2Update :: Accum2 -> (Loc,(String,BrackType)) -> Accum2
accum2Update a (loc,(staffN,bt)) = case bt of
  BrackL brackName -> accum2UpdateL a loc staffN brackName
  BrackR brackName -> accum2UpdateR a loc staffN brackName


accum2UpdateL :: Accum2 -> Loc -> String -> String -> Accum2
accum2UpdateL a loc staffN brackName = case M.lookup staffN a of
  Just x -> case x of
    Nothing -> M.insert staffN (Just (loc,brackName)) a


accum2UpdateR :: Accum2 -> Loc -> String -> String -> (Bd,Accum2)
accum2UpdateR a loc staffN brackName = case M.lookup staffN a of
  Just x -> case x of
    Just (leftLoc,leftBrackName)
      | brackName == leftBrackName -> ( (brackName,(leftLoc,loc))
                                      , M.insert staffN Nothing a )


mapAccumBracketData :: Map String (Map Loc AtLocStaff) ->
                       Map Loc (Map String (Maybe Bd))
mapAccumBracketData m = error "foo"
  where
    fm :: Map Loc (Map String AtLocStaff)
    fm = flipMap m

accumLocKeys :: Accum2 -> Loc -> Map String AtLocStaff ->
                (Accum2,Map String (Maybe Bd))
accumLocKeys a loc m = M.mapAccumWithKey (accumStringKeys loc) a m


accumStringKeys :: Loc -> Accum2 -> String -> AtLocStaff -> (Accum2,Maybe Bd)
accumStringKeys loc a staffN (AtLocStaff _ mL mR) = case mR of
  Just rightBrackName ->
    let (bd,aOut) = accum2UpdateR a loc staffN rightBrackName
    in (aOut,Just bd)
    


{-
-- mapAccumBracketData
--
-- Given Map of AtLocStaff, one for each staff, produce bracket data for each
-- staff by calling mapAccumBracketData1, then merge them into BdAll.
mapAccumBracketData :: Map String (Map Loc AtLocStaff) -> BdAll
mapAccumBracketData = M.map g 
  where
    g :: Map Loc AtLocStaff -> [(String,(Loc,Loc))]
    g = catMaybes . M.elems . mapAccumBracketData1
    validate :: BdAll -> BdAll
    validate m | all h x = m
      where
        allTails :: [[(String,[Bd])]]
        allTails = L.tails $ M.toAscList m
        h :: [(String,[Bd])] -> Bool
        h [] = True
        h (x:xs) = 

-- mapAccumBracketData1
--
-- Given Map of all AtLocStaff on one staff, produce a map giving all
-- bracket data (i.e. bracket names and Loc spans) in the form of elements
-- of Maybe Bd.
mapAccumBracketData1 :: Map Loc AtLocStaff -> Map Loc (Maybe Bd)
mapAccumBracketData1 m = snd $ M.mapAccumWithKey f Nothing m
  where
    f :: Accum1 -> Loc -> AtLocStaff -> (Accum1,Maybe Bd) 
    f p loc (AtLocStaff _ mL mR) = case p of
      Nothing -> case (mL,mR) of
        (Nothing,Nothing) -> (Nothing,Nothing)
        (Just s ,Nothing) -> (Just (loc,s),Nothing)
      Just (locP,sP) -> case (mL,mR) of
        (Nothing,Nothing) -> (Nothing,Nothing)
        (Nothing,Just r ) | r == sP -> (Nothing,Just (r,(locP,loc)))
        (Just l ,Just r ) | r == sP -> (Just (loc,l),Just (r,(locP,loc)))
-}

oneAtLocStaff :: String -> Loc -> [Mark3] ->
                 (Maybe String, Maybe String, Maybe String)
oneAtLocStaff staffN loc ms = (g isArtic,g isBracketL,g isBracketR)
  where
    g :: (Mark3 -> Maybe String) -> Maybe String
    g h = case mapMaybe h ms of
      [] -> Nothing
      [s] -> Just s
      _   -> throwMine $ printf ("Synth: two or more marks (Artic, " ++
             "BracketL, or BracketR) on staff %s at %s") staffN (showLoc2 loc)


foldBrackets :: ArticBracketSections -> Map String (Map Loc AtLocStaff) ->
                ArticBracketSections
foldBrackets absIn m = foldl oneStaff absIn $ M.toAscList m
  where
    oneStaff :: ArticBracketSections -> (String,Map Loc AtLocStaff) ->
                ArticBracketSections
    oneStaff abs (staffN,m2) = fbdArticBracketSections out
      where
        out = foldl oneLoc (FoldBracketData abs Nothing) (M.toAscList m2)
        oneLoc :: FoldBracketData -> (Loc,AtLocStaff) -> FoldBracketData
        oneLoc fbd2@(FoldBracketData abs2 mOpen) (loc,als) = error "foo"
          where
            out = case (alsBracketL als,alsBracketR als) of
              (Nothing,Nothing) -> fbd2
              (Just l ,_      ) | isNothing mOpen ->
                                    fbd2 { fbdOpenBracket = Just (l,loc) }
              (Nothing,Just r ) -> case mOpen of
                Just (openName,locLeft) ->
                  fbd2 { fbdArticBracketSections =
                           insertBracket staffN locLeft loc abs2
                       , fbdOpenBracket = Nothing }

                  

insertBracket :: String -> Loc -> Loc -> ArticBracketSections ->
                 ArticBracketSections
insertBracket staffN loc1 loc2 (ArticBracketSections m1 m2) = error "foo"


-- data BrData = BrData (Maybe String) (Maybe String)


{-

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

-}

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
