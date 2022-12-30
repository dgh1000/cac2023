
import qualified Data.Map as M
import Data.Map(Map)
import Common
import Control.Monad
import Control.Monad.State
import Score.ScoreData
import Uvi

-- the problem with translating note-by-note is that we don't see context. okay so a staff has
-- an associated AInstr, but the staff may have sections. call these "sound sections." 
  
doStaff :: AInstr -> Staff -> Ut [UtNote]
doStaff ainstr staff = do
  score <- gets usScore
  concat `liftM` (mapM (doLoc ainstr) $ M.toAscList $ stChords staff)


doLoc :: AInstr -> (Loc,Map Int Chord) -> Ut [UtNote]
doLoc ainstr (loc,chords) = concat `liftM` (mapM (doChord loc) $ M.toList chords)


doChord :: AInstr -> Loc -> (Int,Chord) -> Ut [UtNote]
doChord ainstr loc (vn,chord) = concat `liftM` (mapM (doNote loc vn chord) cNotes)


