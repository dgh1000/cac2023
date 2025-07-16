
module Configs.WTC4_v02 where

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

makeFn :: String -> BracketFunc
makeFn s = simpleBracketFunc $
  SimpleBracketFunc s M.empty 0.05 velCurve1 Nothing

c1BF = mkFn "c1"
c2BF = mkFn "c2"
c3BF = mkFn "c3"
c4BF = mkFn "c4"
c5BF = mkFn "c5"
c6BF = mkFn "c6"
c7BF = mkFn "c7"
c8BF = mkFn "c8"


-- what do the bracket functions do? send to a specific channel


staffNs =
         [ "Bassoon"
         , "Clarinet in Bb"
         , "Flute"
         , "Horn in F"
         , "Oboe"
         , "S1"
         , "S2"
         , "S3"
         , "S4"
         , "S5"
         , "S6"
         , "S7"
         , "S8"
         ]

bfs = M.fromList
  [ ("c1", c1BF )
  , ("c2", c2BF )
  , ("c3", c3BF )
  , ("c4", c4BF )
  , ("c5", c5BF )
  , ("c6", c6BF )
  , ("c7", c7BF )
  , ("c8", c8BF ) ]


chanDestNames =
  [ ("c1" , (0, 1))   -- 1
  , ("c2" , (0, 2))   -- 2
  , ("c3" , (0, 3))   -- 3
  , ("c4" , (0, 4))   -- 4
  , ("c5" , (0, 5))   -- 5
  , ("c6" , (0, 6)) 
  , ("c7" , (0, 7)) ]


makeChan :: String -> (Int,Int) -> (String,SynthChan)
makeChan chanName dest = (chanName,SynthChan dest M.empty M.empty)


wtc4Synth =
  makeSynth2
    "wtc4"
    staffNs
    (M.fromList $ map (uncurry makeChan) chanDestNames)
    bfs
    gsFunc1
