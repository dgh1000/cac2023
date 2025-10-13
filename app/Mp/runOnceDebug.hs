
import qualified Data.Map as M
import Instruments.Piano
import Instruments
import Translation
import System.Environment
import Translation.RunOnce
import Translation.GenericShape
import Data.Ratio
import Configs.GenericShapeMasterFunctions
import Common


instr2 = makePiano "piano-instr"
         0.1
         (M.fromList [ ("Piano", (0,1)) ])
         0.75
         (VelCurve [ (0.45,10)
                   , (8.55,110) ])
         (VelCurve [ (0.45,10)
                   , (8.55,100) ])
         127
         gsFunc1
         True

-- main = getArgs 
--   >>= runOnce (RunData [instr2])

runData = RunData [instr2]
main = doPlay 2 Nothing Nothing [] runData