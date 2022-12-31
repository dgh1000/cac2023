module Score.XmlToScore_ties where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Map(Map)
import Data.Set(Set)
import Data.List.Index
import Common
import XmlDoc.XmlDocData

data TieEnd = TieEnd 
  { tePitch :: Pitch
  , teVoice :: Int
  , teStaff :: Maybe Int
  , teBegin :: Loc
  , teEnd   :: Loc
  }

data TNote = TNote 
  { tnPitch    :: Pitch
  , tnVoice    :: Int
  , tnStaff    :: Maybe Int
  , tnTieStart :: Bool
  , tnTieEnd   :: Bool
  , tnBegin    :: Loc
  , tnEnd      :: Loc
  , tnOrder     :: Int  -- index into the order this note appeared 
                        -- in the XMsr
  , tnNotations :: [XNotation]
  , tnNotehead  :: Maybe XNotehead
  }

type MLT = Map Loc [TNote]
-- (s -> a -> (s, b)) -> s -> t a -> (s, t b) 
-- mapAccumL :: (s -> a -> (s, b)) -> s -> [a] -> (s,[b])

-- If we sort TNotes by grace note first, Loc second, 
-- and order within measure third, then we are set up to process one
-- TNote at a time in backwards order
-- 

step3 :: Map Loc [TieEnd] -> TNote -> (Map Loc [TieEnd], Maybe TNote)
step3 tes tn = out
  where
    out = case tnTieStart tn of
      False -> (tes, Just tn)
      True  -> stepHandleTie tes tn



stepHandleTie :: Map Loc [TieEnd] -> TNote -> (Map Loc [TieEnd], Maybe TNote)
stepHandleTie tes tn = out
  where
    out = case M.lookup (tnEnd tn) tes of
      Nothing -> (tes,Just tn)
      Just teList -> 
        let p = tnPitch tn
            matchingTes = filterMatchPitch p teList
            matchingTesVoice = filterMatchVoice (tnVoice tn) matchingTes
            matchingTesStaff = filterMatchStaff (tnStaff tn) matchingTes
            maybeTieEnd | tnTieEnd tn = Nothing
                        | otherwise   = Just tn
        in
            case (matchingTesVoice,matchingTesStaff) of
              (x:_,_)  -> (updateTesMap tn x tes,maybeTieEnd)
              ([],x:_) -> (updateTesMap tn x tes,maybeTieEnd)

updateTesMap :: TNote -> TieEnd -> Map Loc [TieEnd] -> Map Loc [TieEnd]
updateTesMap tn te map = M.insertWith (++) (tnBegin tn) [teEnd te] map

-- step
--
--  Map Loc [TieEnd] - map of begin Loc to notes that begin at that 
--    Loc and function as tie ends
--  (Loc,[TNote]) - map of notes that end at the given location
--    
step :: Map Loc [TieEnd] -> (Loc,[TNote]) -> (Map Loc [TieEnd],[TNote])
step = error "foo"

-- cases:
--   1. tn is neither tie start nor tie end
--   2. tn is tie start
--   3. tn is tie end
step2 :: Map Loc [TieEnd] -> TNote -> (Map Loc [TieEnd],Maybe TNote)
step2 mtes tn 
  | not (tnTieStart tn) = (mtes,Just tn)
  | otherwise     = error "foo"
  where



filterTieStart :: [TNote] -> [TNote]
filterTieStart = filter tnTieStart

filterMatchPitch :: Pitch -> [TieEnd] -> [TieEnd]
filterMatchPitch (Pitch midi _ _ _) = 
  filter (\e -> midi == (midiPitch . tePitch $ e))
  
filterMatchVoice :: Int -> [TieEnd] -> [TieEnd]
filterMatchVoice vn = filter (\e -> vn == teVoice e)

filterMatchStaff :: Maybe Int -> [TieEnd] -> [TieEnd]
filterMatchStaff mStaff = filter (\e -> mStaff == teStaff e)

pairOff :: (a -> b -> Int) -> a -> [b] -> [b] -> (Maybe c,Maybe a)
pairOff = error "foo"
