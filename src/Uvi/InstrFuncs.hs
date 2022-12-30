
module Uvi.InstrFuncs where

import qualified Data.Map as M
import Control.Monad
import Score.ScoreData
import Common
import Uvi


convPatchSection :: AInstr -> PatchSection -> Ut [UtNote]
convPatchSection ai = convPatchSection_ai ai


convPatchSection_ai :: AInstr -> PatchSection -> Ut [UtNote]
convPatchSection_ai ai ps = aiToNotes ai ai ps

sectToNotes_Ai_IrTrumpet :: AInstr -> PatchSection -> Ut [UtNote]
sectToNotes_Ai_IrTrumpet (AInstr _ cInstrs selectCInstr toNotes) ps = do
  return []

convPatchSection_chord :: AInstr -> PatchSection -> ChordData -> Ut [UtNote]
convPatchSection_chord ai ps (ChordData staffN loc vn chord notes) = do
  tBeg <- lookupTimeUt staffN loc
  let doNote (_,note) = do
        tEnd <- lookupTimeUt staffN $ nTrueEnd note
        ci   <- aiSelectCInstr ai ai ps loc
        let n = UtNote staffN tBeg tEnd ci
        return [n]
  case notes of
    NSingles m -> concat `liftM` mapM doNote (M.toList m)
  

lookupTimeUt :: String -> Loc -> Ut Double
lookupTimeUt staffN loc = error "lookupTimeUt"

-- convert a chord. convert the individual notes.
