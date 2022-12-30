{-# LANGUAGE ExistentialQuantification #-}

module Cac.Simpler2 where

import Data.Map.Strict(Map)
import qualified Data.Map.Strict as M
import qualified Data.Set as S

----------------------------------------------------------------------

data Comp = Comp {
  cNotes   :: Map Int [Note]
  }

----------------------------------------------------------------------

data Note = Note
  { nPitch  :: Int
  , nTimes  :: Times
  , nDyn    :: Double
  , nInstr  :: String
  , nParams :: Map String Double
  }


data MidiNote = MidiNote
  { mnPitch :: Int
  , mnTimes :: Times
  , mnDest  :: (Int,Int)
  , mnVel   :: Int
  }


type ToMidiNoteFunc = Note -> Maybe MidiNote

data Instr = Instr
  { insConv :: Note -> Either [Note] [MidiNote]
  }


data Times = Times
  { tOn  :: Int
  , tOff :: Int
  }


class MotiveClass m where
  motiveLen  :: m -> Int
  motiveElem :: Int -> Int -> m -> Int  -- direction -> n -> motive -> elem

  
{- 
data MotivePit = MotivePit
  { mpType   :: MotivePitType
  , mpElems  :: [Int]
  }
-}

data MotivePitType = MptAbsolute | MptRelative



data Motive e = Motive
  { mElems :: [e]
  , mRep   :: RepetitionConfig
  }


-- motive concepts
--
--   motive has N elements in order, with element index ranging from 0 to idx.
--
--   motive is "deployed," that is the elements are chosen in a particular
--   order, which is done by moving a pointer forward and back.
--
--     a partial or complete deployed motive is a list of index values
--
--   element instance: a deployed motive is a list of element instances
--
--   ptr history: list of values the ptr has taken on, in order
-- 
--   computing travel factor
--
--     in every step of history, looking at history, every ptr is a certain
--     number of steps back from furthest advance. 0 to whatever. for each
--     step in the motive deployment.


data RepetitionConfig = RepetitionConfig
  { rcTravelRange     :: (Double,Double)
  , rcRepetitionRange :: (Double,Double) 
  }


data MotivePtr = MotivePtr
  { mpLen     :: Int
  , mpIdx     :: Int
  , mpHistory :: [Int]
  }


mpNexts :: Motive e -> MotivePtr -> ([e],Bool)
mpNexts mot motPtr = error "foo"
  where
    


listLookupHelp :: Int -> Int -> [Int] -> Int
listLookupHelp dir idx es
  | 0 <= idx && idx < length es =
      case dir of
        1  -> es !! idx
        -1 -> es !! (length es-1-idx)
  

data MotiveDyn = MotiveDyn
  { mdElems   :: [Int]
  }

data MotiveDur = MotiveDur
  { mdrElems  :: [Int]
  }

----------------------------------------------------------------------

data Counter m = Counter
  { cntMotive    :: m
  , cntPosition  :: Int
  , cntHistory   :: [Int]
  , cntDirection :: Int
  , cntRepDens   :: RepeatDensity
  }


data RepeatDensity = RepeatDensity
  { rdRanges   :: Map Int (Double,Double)
  }

-- counterIsDone
--
-- position indicates next choice. moves from 0 to n-1 where n is length of
-- motive. when it reaches n, counter is done
counterIsDone :: MotiveClass m => Counter m -> Bool
counterIsDone (Counter m pos _ _ _) = pos >= motiveLen m


counterLkElem :: MotiveClass m => Counter m -> Int 
counterLkElem = error "lookupElem"



----------------------------------------------------------------------



possibleTimes :: Int -> Comp -> Motive -> MotivePtr -> [Int]
possibleTimes nBack comp motive motPtr = error "foo"
  where
    


