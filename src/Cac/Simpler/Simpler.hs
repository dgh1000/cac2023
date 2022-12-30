module Cac.Simpler where

import Data.Map.Strict(Map)
import Util.Curve
import Common

-- different approach to c.a.c.
--
-- music that is simpler, less sophisticated, for general consumption.
--
-- arpeggios. diatonic, pantonality. motives.
--
-- what do we need at a minium? a motive
--
-- a sequence of loud/soft, high/low, short/long
--
--
-- besides motive, we need a beat. probably something in 3, 5, irregular. we
-- could do beat first -- hierarchical beat? what word besides beat? how about
-- beat pattern? bpat? 


-- abstractions in a computer-aided composition
--
-- a list of staves
--
-- a "staff" contains
--
--   list or map of AbNotes
--
--   list of AbCtrlChan's : abstrct controller channels
--
--     could be used for "brightness," "edginess," "loudness"
--
--     all AbCtrlChan's on a staff will have unique names
--
--     an AbCtrlChan has segments. curve is only defined within segments.
--
-- algorithm mapping staves to MIDI channels
--
--   for one staff S with following properties
--
--     set N of notes
--
--       configured with instrument name as a string
--
--     set AC of abstract control channels
--
--       each one has segments S_1 through S_n
--
--   for each note n1, at time t1:
--
--     find n1's instrument, Instr1. verify that it's a "direct instrument"
--     (goes to one MIDI channel). look up associated MIDI channel.
--
--     find any segments in control channels overlapping with the duration of
--     n1: call these S_o
--
--     Instr1 will compute MIDI velocity based on values in S_o at time t1. It
--     will also determine MIDI destination and pitch.
--
--     Instr1 will determine if any segments in S_o need to be sent to MIDI
--     control values, and which values those are. 
--
-- how does staff relate to MIDI channel?
--
--   a MIDI channel will have a MIDI instrument. each staff may have notes
--   mapped to any of several MIDI instruments.
--
--   a staff has one or more abstract control channels. These will get mapped
--   to MIDI controllers on MIDI channels. an AbCtrlChan could get mapped to
--   different MIDI control channels for different points in time, depending
--   on which instrument is sounding at that time



data Motive = Motive [MComponent]

data MComponent = MComponet [MElem]

data MElem = MeLoudness
           | MeDuration
           | MeRegister


data BPat = BpAttr [BAttr]
          | BpNode [BPat]


data BAttr = BaLoudness Double   -- 0 to 9
           | BaDur      Double
           | BaTimbre   BeatTimbre


data BeatTimbre = BtCymbal | BtLowDrum | BtSnare


mkBAttr dur loud timbre = BpAttr [ BaLoudness loud
                                 , BaDur      dur
                                 , BaTimbre   timbre ]

bPat1 = BpNode [ mkBAttr 1 4 BtLowDrum
               , mkBAttr 1 7 BtSnare
               , mkBAttr 1 4 BtLowDrum
               , mkBAttr 1 7 BtSnare ]



data Timing = Timing { tOn :: Double, tOff :: Double, tDescr :: String }

data TimeHistory = TimeHistory [Timing]

-- Instr
--
--   An "instrument." Converts an abstract note (AbNote) to either more
--   abstract notes or to midi notes (MNotes).
--
--   Consider an abstract note, together in context with an Instr, which
--   specifies a trill, arpeggio, or other pattern made of individual notes or
--   sounds. Then the Instr would convert the AbNote to a list of additional
--   AbNotes.
--
--   Some AbNotes specify only a single sound. Those would be converted
--   directly to MNotes.
data Instr = Instr
  { insConvert :: AbNote -> Either [AbNote] [MNote]
  , insChannel :: CtrlChan -> [MCtrl]
  }

data AbNote = AbNote
  { inInstr      :: String
  , inTime       :: (Double,Double)
  , inTOff       :: Double
  , inPitch      :: Int 
  , inLoud       :: Double
  }

data MNote = MNote
  { mnInstr   :: String
  , mnDest    :: (Int,Int)
  , mnTime    :: (Double,Double)
  , mnVel     :: Int
  }


data MCtrl = MCtrl
  { mcDest    :: (Int,Int)
  , mcCtrlNum :: Int
  , mcValue   :: Int
  }

-- type Instrument = BpAttr -> [SNote]

data Track = Track
  { trNotes :: Map Double [AbNote]
  , trCtrlChans :: Map String CtrlChan
  }


data CtrlChan = CtrlChan
  { ccName   :: String
  , ccCurve  :: UtilCurve
  }


data Comp = Comp
  { cTracks :: Map String Track
  }

compToMidi :: Comp -> Map String Instr -> [MNote]
compToMidi comp instrMap = error "foo"


data InstrConfig01 = InstrConfig01
  { ic01Dest      :: (Int,Int)
  , ic01VelCurve  :: VelCurve
  , ic01CtrlFuncs :: Map String CtrlFunc
  }

-- controls
--
--   the algorithm will generate control curves that fit generic notions, such
--   as "brightness" or "edginess" or "thinness"
--
--   staves
--   
--   at the most concrete level, we have 

instr1Convert = error "foo"

