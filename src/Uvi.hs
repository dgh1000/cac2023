
-- do we need to translate in a serial fashion? what steps need to be done? for each note we
-- need to decide what channel to send it to. we also need to create curves. modulate. we need
-- to decide on keyswitches. should this be done in serial fashion, one note at a time?
-- probably not. also need possibly to share staves. need to invent new marks, possibly. start
-- by creating full name for them with macros, full command name, then later can come up with
-- abbreviation

-- 


-- model of instruments:
--
--   AInstr (acoustic instrument):
--
--     (doesn't have to be acoustic, but often an AInstr will be something like "a trumpet")
--
--     the biggest abstraction of instrument. there is one AInstr per staff.
--
--     an AInstr can make various sounds, called 'patches'
--
--       for IRCAM trumpet, one patch might be fluttertongue
--
--  CInstr (channel instrument):
--
--     associates a particular midi dest (stream and channel numbers) with the sampler or
--     synthesizer at that dest
--

module Uvi where

import Control.Monad.Except
import Control.Monad.State
import System.Random
import Common
import Data.Map(Map)
import Score.ScoreData
import Instruments

data UviRunData = UviRunData
  { urdAInstrs :: Map String AInstr
  }

type Ut = ExceptT String (State UtState)

runUt m s = runState (runExceptT m) s

data VelCurve = VelCurve [(Double,Double)]


{-
data SInstr = SInstr
  { siAInstr :: AInstr
  }
-}

data AInstr = AInstr
  { aiName         :: String
  , aiCInstrs      :: [CInstr]
  , aiSelectCInstr :: AInstr -> PatchSection -> Loc -> Ut CInstr
  , aiToNotes      :: AInstr -> PatchSection -> Ut [UtNote]
  }


data CInstr = CInstr
  { ciName :: String
  , ciDest :: (Int,Int)
  , ciUArtics :: [UArtic]
  }


data UArticCategory = UcOrdinario
                    | UcStaccato
                    | UcSforzando
                    | UcPizz

                
data UArtic = UArtic
  { uaName        :: String
  , uaVelCurve    :: VelCurve
  , uaModResponse :: Bool
  , uaVelResponse :: Bool
  , uaCategory    :: UArticCategory
  }

data UtNote = UtNote
  { unStaffName :: String
  , unTBeg      :: Double
  , unTEnd      :: Double
  , unCInstr    :: CInstr
  }

data UtRaw = UtRaw
  { urPitch :: Int
  }
    

data UtState = UtState
  { usScore        :: Score
  , usRange        :: (Int,Int)
  , usTempoRatio   :: Double
  , usAInstrs      :: Map String AInstr
  , usGen          :: StdGen
  , usTimeMaps     :: Map String AbsTimeMap
  , usLoudness     :: Map String (Map Int Curve)
  , usNotes        :: [[UtNote]]
  , usInitRaws     :: [[UtRaw]]
  , usRaws         :: [[UtRaw]]
  , usTimeMods     :: Map String [UnitTimeMod]
  }


-- 2017-08-05: thinking about where I left off... so patch section appears to have the patch
-- name and Locs... but a good deal other stuff which would be useful.. where is my code that
-- constructs PatchSections?

data PatchSection = PatchSection
  { psPatchName :: String
  , psStaff     :: Staff
  , psMarks     :: Map Loc [Mark Double] -- marks with location x such that beg <= x < end
  , psLocBeg    :: Loc
  , psLocEnd    :: Loc
  , psKeys      :: [ChordData]
  }


data ChordData = ChordData
  { nkStaffName   :: String
  , nkLoc         :: Loc
  , nkVoiceNum    :: Int
  , nkChord       :: Chord
  , nkNotes       :: Notes
  }


  
