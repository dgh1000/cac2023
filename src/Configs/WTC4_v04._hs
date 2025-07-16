module Configs.WTC4_v04 where

import Common
import Configs.GenericShapeMasterFunctions
import qualified Data.Map as M
import Instruments
import Instruments.Piano
import Translation

velCurve1 = VelCurve [(0.45, 1), (8.55, 127)]

velCurve2 = VelCurve [(0.45, 40), (8, 127)]

-- what do I remember about HIE? probably need to look it up - 

mkFn2 :: (String, Int) -> MetaInstr
mkFn2 (staffName, chan) =
  makePiano
    staffName
    0.1
    (M.fromList [(staffName, (0, chan))])
    0.75
    (VelCurve [(0.45, 10), (8.55, 110)])
    (VelCurve [(0.45, 10), (8.55, 100)])
    127
    gsFunc1
    False

-- what do the bracket functions do? send to a specific channel
staffNs =
  [ ("Bassoon", 16)
  , ("Clarinet in Bb", 16)
  , ("Flute", 16)
  , ("Horn in F", 16)
  , ("Oboe", 16)
  , ("Piano1", 1)
  , ("Piano2", 1)
  , ("Piano3", 1)
  , ("Harpsichord1", 6)
  , ("Harpsichord2", 6)
  , ("Harpsichord3", 6)
  ]

wtc4_v04_instrs = map mkFn2 staffNs
