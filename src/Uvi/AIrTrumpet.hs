module Uvi.AIrTrumpet where


-- how does a note within a chord change into MIDI data?
--
-- first convert into an abstraction
--
-- if note is not part of trill
--
--   pitch: as specified in Score
--
--   begin time and end time: as its Loc and true end Loc mapped to Double using staff time map
--
--   


import qualified Data.Map as M
import Control.Monad
import Uvi
import Score.ScoreData
import Uvi.InstrFuncs

-- to convert a patch 

convPatchSection_ai :: AInstr -> PatchSection -> Ut [UtNote]
convPatchSection_ai ai ps = do
  -- convert individual chords
  return []

convChordData :: AInstr -> PatchSection -> ChordData -> Ut [UtNote]
convChordData ai ps (ChordData staffN loc vn chord notes) = do
  tBeg <- lookupTimeUt staffN loc
  let doNote (_,note) = do
        tEnd <- lookupTimeUt staffN $ nTrueEnd note
        ci   <- aiSelectCInstr ai ai ps loc
        let n = UtNote staffN tBeg tEnd ci
        return [n]
  case notes of
    NSingles m -> concat `liftM` mapM doNote (M.toList m)
  

