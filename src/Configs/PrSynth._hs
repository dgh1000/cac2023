-- despite this being called "PrSYNTH" it's a file I've been using for
-- playback with QL Orch

module Configs.PrSynth where

import qualified Data.Map as M
import Debug.Trace
import Instruments.Piano
import Instruments.InstrumentsData
import System.Environment
import Instruments.RunOnce
import Instruments.GenericShape
import Common
import Instruments.AnyIrcam
import Instruments.AnyQ

{-
instr2 = makeSynth [ "T1"
                   , "B1"
                   , "T2"
                   , "B2" ]
         gsFunc1
         0.6
         (VelCurve [ (0, 1)
                   , (9, 127) ])
         [("a",(0,1)),("b",(0,1)),("c",(0,3)),("d",(0,4)),("e",(1,1))]
-}

instrPno = makePiano "instr-piano"
         0.05
         (M.fromList [ ("B2",(3,1))])
         0.6
         (VelCurve [ (0.95,10)
                   , (8.05,110) ])
         (VelCurve [ (0.95,10)
                   , (8.05,100) ])
         127
         gsFunc1


thisMain = getArgs
     >>= runOnce (RunData [ qFlute  "T1" (1,4) (1,3)
                          , qViolin "B1" (0,2) (0,1)
                          , irGtr   "T2" (2,1)      127
                          , instrPno ])

