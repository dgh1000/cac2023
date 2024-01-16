{-# LANGUAGE ScopedTypeVariables, FlexibleContexts #-}

module Cac.Markov.Run where

import qualified Data.Map as M
import Data.Map(Map)
import Cac.Markov.Data
import Control.Lens
import Control.Monad.State

-- choose follow node
--
--
-- node fields: the pitch, dur, timbre, etc. contained in a node
--
-- to "consume" node information: to create a note from it
--
-- to "step from" a node: to decide which edge to follow
--
-- to "advance from" a node: to consume it, then step from it
--
-- current pointer state (CPS):
--
--   a CPS "points to" a node: we hold the id of the node being pointed to
--
--   when we point to a node, we are waiting to consume that information and
--   advance
--
--   maybe tNext (if we are in a network that provides span, this will be the
--               time that next note will be emitted from current node)
--
-- network set: combinatiion of networks N_1, N_2, etc. Each network provides
--    a set of fields F_1, F_2, etc. The sets F1, F2, etc. have a null
--    intersection in every pair, and the union provides all the fields of a
--    Note.
--
-- parallel network sets: generate music in parallel, perhaps different
-- timbres
--
-- composing process
--
--   for each network set
--
--     initialize CPS-set
--
--        that means a CPS is initialized for every network, CPS_1, CPS_2,
--        etc. They point to nodes N_1, N_2, etc. Together the nodes form a
--        note and the time it is to be emitted. We create and store that
--        note.
--
--   on every step, emit node


-- 
run01 :: M es ()
run01 = do
  netSets <- msNetworkSetList `fmap` get
  let netSetIn = case netSets of
        []  -> error "186730"
        n:_ -> n
  (note', netSet') <- runOneNetworkSet netSetIn
  modify (\st -> st { msNotes = msNotes st ++ [note']
                    , msNetworkSetList = [netSet'] })

-- runOneNetworkSet
--
--   emits one note and advances state including to next time.
--     
runOneNetworkSet :: forall es. NetworkSet es -> M es (Note,NetworkSet es)
runOneNetworkSet netSet = do
  ts :: [(NoteData,Network es)]
    <- mapM (runOneNetwork $ view followFunc netSet)
            (view networks netSet)
  let (noteDataList,networkList) = unzip ts
      combinedNoteData = case combineManyNoteData noteDataList of
        Left s   -> error s
        Right nd -> nd
      newTSpan = runFromMaybe "runOneNetworkSet newTSpan" $
                 view tSpan combinedNoteData
      (noteOut,newTNext) = noteDataToNote (view nextT netSet) combinedNoteData
      newNetSet = set networks networkList .
                  set nextT newTNext $
                  netSet
  return (noteOut, newNetSet)


runOneNetwork :: forall es. FollowFunc es -> Network es ->
                 M es (NoteData,Network es)
runOneNetwork followFunc nw@(Network _ _nodes _ptrStates) = do
  let currNode = rLookup _ptrStates _nodes
  nextId :: Int <- followFunc nw currNode 
  let nw2 = set ptrStates nextId nw
  return (nodeToNoteData currNode,set ptrStates nextId nw)


noteDataToNote :: Double -> NoteData -> (Note,Double)
noteDataToNote
  tNow
  (NoteData
     idens mTSpan mTDur mPitch mAmpl mTimbre) =
  (Note idens tNow (tNow+tDur) pitch ampl timbre, tNow+tSpan)
    where
      rFromMaybe :: String -> Maybe a -> a
      rFromMaybe _ (Just x) = x
      rFromMaybe s Nothing  = error $ "in noteDataToNote, field " ++ s
      tSpan = rFromMaybe  "tSpan"  mTSpan
      pitch = rFromMaybe  "pitch"  mPitch
      ampl  = rFromMaybe  "ampl"   mAmpl
      timbre = rFromMaybe "timbre" mTimbre
      tDur   = rFromMaybe "dur"    mTDur


runFromMaybe :: String -> Maybe a -> a
runFromMaybe _ (Just x) = x
runFromMaybe s Nothing  = error s


-- 
combineManyNoteData :: [NoteData] -> Either String NoteData
combineManyNoteData ns = foldl (>>=) (Right emptyNodeData) nns
  where
    nns :: [NoteData -> Either String NoteData]
    nns = map combineNoteData ns


rLookup :: Ord k => k -> Map k a -> a
rLookup k m = case M.lookup k m of {Just x -> x}
 

nodeToNoteData :: Node es -> NoteData
nodeToNoteData node = 
  NoteData []
           (view tSpan   node)
           (view dur     node)
           (view pitch   node)
           (view ampl    node)
           (view timbre  node)


combineMaybe :: String -> Maybe a -> Maybe a -> Either String (Maybe a)
combineMaybe _       (Just x)  Nothing  = Right (Just x)
combineMaybe _       Nothing   (Just x) = Right (Just x)
combineMaybe msg     (Just _)  (Just _) =
  Left $ "In combine Maybe, two Justs: "    ++ msg
combineMaybe msg     Nothing   Nothing =  Right Nothing
  --   Left $ "In combine Maybe, two Nothings: " ++ msg


combineNoteData :: NoteData -> NoteData -> Either String NoteData
combineNoteData
  (NoteData idens1 ts1 dur1 pit1 a1 t1)
  (NoteData idens2 ts2 dur2 pit2 a2 t2)
    =
  do tsc  <- combineMaybe "tSpan"  ts1 ts2
     durc <- combineMaybe "dur"    dur1 dur2
     pitc <- combineMaybe "pitch"  pit1 pit2
     ac   <- combineMaybe "ampl"   a1 a2
     tc   <- combineMaybe "tibre"  t1 t2
     return $ NoteData (idens1 ++ idens2) tsc durc pitc ac tc

