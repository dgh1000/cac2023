
module Configs.WTC4 where

import qualified Data.Map as M
import Translation
import Common
import Instruments
import Instruments.SynthExamples
import Instruments.Synth hiding (simpleBracketFunc)
import Configs.GenericShapeMasterFunctions

velCurve1 = VelCurve [ (0.45,  1)
                     , (8.55,127) ]


velCurve2 = VelCurve [ (0.45,  40)
                     , (8   , 127) ]

pianoBF = simpleBracketFunc $
  SimpleBracketFunc "piano" M.empty 0.05 velCurve1 Nothing


vibraphoneBF = simpleBracketFunc $
  SimpleBracketFunc "vibraphone" M.empty 0.05 velCurve1 Nothing


maribaBF = simpleBracketFunc $
  SimpleBracketFunc "mariba" M.empty 0.05 velCurve1 Nothing


xylophoneBF = simpleBracketFunc $
  SimpleBracketFunc "xylophone" M.empty 0.05 velCurve1 Nothing

  
harpBF = simpleBracketFunc $
  SimpleBracketFunc "harp" M.empty 0.05 velCurve1 Nothing

  
harpsichordLBF = simpleBracketFunc $
  SimpleBracketFunc "harpsichordL" M.empty 0.05 velCurve2 (Just (7, velCurve2))

harpsichordUBF = simpleBracketFunc $
  SimpleBracketFunc "harpsichordU" M.empty 0.05 velCurve2 (Just (7, velCurve2))


staffNs =
         [ "Bassoon"
         , "Clarinet in Bb"
         , "Flute"
         , "Horn in F"
         , "Oboe"
         , "Piano1"
         , "Piano2"
         , "Harpsichord1"
         , "Harpsichord2"
         , "Harp1"
         , "Harp2"
         ]

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


makeChan :: String -> (Int,Int) -> (String,SynthChan)
makeChan chanName dest = (chanName,SynthChan dest M.empty M.empty)


wtc4Synth =
  makeSynth2
    "wtc4"
    staffNs
    (M.fromList $ map (uncurry makeChan) chanDestNames)
    bfs
    gsFunc1
