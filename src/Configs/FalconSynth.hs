module Configs.FalconSynth where 
 
import qualified Data.Map.Strict as M
import Common
import Instruments.SynthExamples
import Instruments.InstrumentsData
import Instruments.Synth

-- Create a Falcon synth.
--
-- We need to set up
--
--   long term: sets of control numbers that we re-use in different
--   instruments
--
--   SynthChan's
--
--     we will have multiple dests running
--
--       one Falcon instance can have many "instruments" 
--
--       there may be other Falcon instances or Moog instances
--
--     one SynthChan per dest
--
--     needs named control numbers
--
--       basic10-kb-01: we've set up some control numbers. still need to set up
--       frequency controls on filter
--
--     needs default settings: initially our bracket function may not change
--     any of these defaults
--
--   Bracket functions
--
--     a set of bracket functions is stored with Synth, so those bf's are
--     expected to direct notes to every SynthChan in the Synth
--
--     what are unique about my first bracket functions?
--
--       directed to unique chan name, have unique default values, unique
--       velocity curve, and only some will have loud-modulation curve

falconCtrls01 :: NamedCtrlNums
falconCtrls01 = M.fromList
  [ ("filterADSR", ChanCtrlAdsr 14 15 16 17)
  , ("amplADSR"  , ChanCtrlAdsr 18 19 20 21) ]


falconProgram01 = SynthChan
  { _synthChanChanDest    = (0,1)
  , _synthChanCtrls       = falconCtrls01 
  , _synthChanDefSettings = M.empty
  }

synth01 = Synth
  { _synthChans = M.fromList [("falconChan01", falconProgram01)]
  , _synthBracketFuncs = M.fromList [("a",simpleBracketFunc b1)]
  }

-- what is difference between bracket functioned 

velCurve1 = VelCurve [ (0.45,  1)
                     , (8.55,127) ]

b1 = SimpleBracketFunc "falconChan01" M.empty 0.05 velCurve1 Nothing 

-- makeSynth :: String -> [String] -> Synth -> GsFunc -> MetaInstr
falconSynth01Meta :: [String] -> GsFunc -> MetaInstr
falconSynth01Meta staffNs gsFunc =
  makeSynth "falconSynth01" staffNs synth01 gsFunc

