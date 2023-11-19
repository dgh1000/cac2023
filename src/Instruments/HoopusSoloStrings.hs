module Instruments.HoopusSoloStrings where
import Instruments
import Data.Set(Set)
import qualified Data.Set as S
import Control.Lens
import Translation
import Score.ScoreData
import Translation.Curves
import Translation.GenericShape
import Configs.GenericShapeMasterFunctions
import Common
import Instruments.Hoopus


defVelCurve = VelCurve [(0.9,78),(8.1,127)]

-- for solo violiln, load the following from Hollywood Orchestra Opus Edition
--
-- firstChan + 0: Solo Violin Leg Bow Change Fast with legato switch on
-- firstChan + 1: Solo Violin Martele RRx4
--   for staccato. not continuous mod but continuos expression (11)
-- firstChan + 2: Solo Violin Pizz RRx4
--
-- Set microphone positions to Close V

hoopusSoloViolin :: String -> GsFunc -> Int -> Int -> MetaInstr
hoopusSoloViolin staffN shapeFn stream firstMidiChan =
  MetaInstr staffN [staffN] (hoopusSoloViolinData stream staffN firstMidiChan)
    False runHoopus shapeFn

hoopusSoloViolinData :: Int -> String -> Int -> Hoopus
hoopusSoloViolinData stream staffN firstMidiChan = 
  Hoopus stream staffN (hSoloViolinDetermineChan firstMidiChan)
    -- continuous mod channels
    [ (firstMidiChan+0,(64,127)) ]
    -- continuous express channels
    [ (firstMidiChan+0,(64,127))
    , (firstMidiChan+1,(1 ,127))
    , (firstMidiChan+2,(1 ,127)) ]
    False


hSoloViolinDetermineChan :: Int -> HoopusNoteDescr -> SNote -> HoopusChan
hSoloViolinDetermineChan firstMidiChan noteD s 
  | Staccato `S.member` view modifiers noteD 
      = hSoloViolinStacChan (firstMidiChan + 1) noteD s
  | view markedArtic noteD == "pizz"         
      = hSoloViolinPizzChan (firstMidiChan + 2) noteD s
  | otherwise                                
      = hSoloViolinLegSlurChan (firstMidiChan + 0) noteD s

hSoloViolinStacChan midiChan noteD s = HoopusChan
  midiChan
  (HMCSelector (\_ _ -> 64))
  (HVCLoudness defVelCurve)
  False
  0.6
  

hSoloViolinPizzChan midiChan noteD s = HoopusChan
  midiChan
  (HMCSelector (\_ _ -> 64))
  (HVCLoudness defVelCurve)
  False
  0.6

hSoloViolinLegSlurChan midiChan noteD s = HoopusChan
  midiChan
  HMCContinuousOnly
  -- (HVCFunc (\_ _ -> 64))
  (HVCLoudness defVelCurve)
  False
  0.6

--------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------
-----------------------             solo cello              --------------------------------------


-- for solo cello, load the following from Hollywood Orchestra Opus Edition
--
-- firstChan + 0: Solo cello Leg Bow Change Fast
-- firstChan + 1: Solo cello Martele RRx4
--   for staccato. not continuous mod but continuos expression (11)
-- firstChan + 2: Solo cello Pizz RRx4
--
-- Set microphone positions to Close V

hoopusSoloCello :: String -> GsFunc -> Int -> Int -> MetaInstr
hoopusSoloCello staffN shapeFn stream firstMidiChan =
  MetaInstr staffN [staffN] (hoopusSoloCelloData stream staffN firstMidiChan)
    False runHoopus shapeFn

hoopusSoloCelloData :: Int -> String -> Int -> Hoopus
hoopusSoloCelloData stream staffN firstMidiChan = 
  Hoopus stream staffN (hSoloCelloDetermineChan firstMidiChan)
    -- continuous mod channels
    -- [ (firstMidiChan+0,(64,127)) ]
    []
    -- continuous express channels
    [ (firstMidiChan+0,(48,127))
    , (firstMidiChan+1,(1 ,127))
    , (firstMidiChan+2,(1 ,127)) ]
    False


hSoloCelloDetermineChan :: Int -> HoopusNoteDescr -> SNote -> HoopusChan
hSoloCelloDetermineChan firstMidiChan noteD s 
  | Staccato `S.member` view modifiers noteD 
      = hSoloCelloStacChan (firstMidiChan + 1) noteD s
  | view markedArtic noteD == "pizz"         
      = hSoloCelloPizzChan (firstMidiChan + 2) noteD s
  | otherwise                                
      = hSoloCelloLegSlurChan (firstMidiChan + 0) noteD s

hSoloCelloStacChan midiChan noteD s = HoopusChan
  midiChan
  (HMCSelector (\_ _ -> 64))
  (HVCLoudness defVelCurve)
  False
  0.6
  

hSoloCelloPizzChan midiChan noteD s = HoopusChan
  midiChan
  (HMCSelector (\_ _ -> 64))
  (HVCLoudness defVelCurve)
  False
  0.6

hSoloCelloLegatoVelCurve = VelCurve [(0.99,48),(8.01,127)]

hSoloCelloLegSlurChan midiChan noteD s = HoopusChan
  midiChan
  HMCContinuousOnly
  -- (HVCFunc (\_ _ -> 64))
  (HVCLoudness hSoloCelloLegatoVelCurve)
  False
  0.6
