
module Configs.WTC4 where

import qualified Data.Map as M
import Translation
import Common
import Instruments
import Instruments.SynthExamples
import Instruments.Synth hiding (simpleBracketFunc)
import Configs.GenericShapeMasterFunctions



-- cutoff LP filter, ADSR 1
--
--
--    ctrl 14 - modulation strength
--    ctrl 15 - attack on ADSR1
--    ctrl 16 - decay on ADSR1
--    ctrl 17 - sustain on ADSR1
--    ctrl 18 - release on ADSR1
--
--

ctrlSet1 = [ (14, 20)
           , (15, 20)
           , (16, 20)
           , (17, 20)
           , (18, 20) ]
           
ctrlSet2 = [ (14, 85)
           , (15, 85)
           , (16, 85)
           , (17, 85)
           , (18, 85) ]

velCurve1 = VelCurve [ (0.45,  1)
                     , (8.55,127) ]


velCurve2 = VelCurve [ (0.45,  40)
                     , (8   , 127) ]

bf01 = simpleBracketFunc $
  SimpleBracketFunc "piano" M.empty 0.05 velCurve1 Nothing

staffNs =
         [ "S1"
         , "S2"
         , "S3"
         , "S4"
         , "S5"
         , "S6"
         , "S7"
         , "S8"
         ]

mkChan :: (Int, Int) -> SynthChan
mkChan dest = SynthChan dest M.empty 

bfs = M.fromList
  [ ("p", pianoBF     )
  , ("v", vibraphoneBF)
  , ("m", maribaBF    )
  , ("x", xylophoneBF )
  , ("r", harpBF )
  , ("h1", harpsichordLBF )
  , ("h2", harpsichordUBF ) ]


chanDestNames =
  [ ("piano"        , (0, 1))   -- 1
  , ("vibraphone"   , (0, 2))   -- 2
  , ("marimba"      , (0, 8))   -- 3
  , ("xylophone"    , (0, 8))   -- 4
  , ("harp"         , (0, 5))   -- 5
  , ("harpsichordL" , (0, 6)) 
  , ("harpsichordU" , (0, 7)) ]

