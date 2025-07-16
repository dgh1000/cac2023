
module Configs.WTC4_v03 where

import qualified Data.Map                      as M
import           Translation
import           Common
import           Instruments
import           Instruments.Piano
import           Configs.GenericShapeMasterFunctions

velCurve1 = VelCurve [(0.45, 1), (8.55, 127)]


velCurve2 = VelCurve [(0.45, 40), (8, 127)]

mkFn2 :: (String,Int) -> MetaInstr
mkFn2 (staffName,chan) = makePiano staffName
  0.1
  (M.fromList [(staffName, (0,chan))])
  0.75
  (VelCurve [(0.45, 10), (8.55, 110)])
  (VelCurve [(0.45, 10), (8.55, 100)])
  127
  gsFunc1
  True




-- what do the bracket functions do? send to a specific channel


staffNs =
  [ ("Bassoon", 16)
  , ("Clarinet in Bb", 16)
  , ("Flute", 16)
  , ("Horn in F", 16)
  , ("Oboe", 16)
  , ("Piano1", 1)
  , ("Piano2", 1)
  , ("Vibraphone", 2)
  , ("Marimba", 3)
  , ("Xylophone", 4)
  , ("Harp", 5)
  , ("Harpsichord1", 6)
  , ("Harpsichord2", 7)
  ]


wtc4_v03_instrs = map mkFn2 staffNs
