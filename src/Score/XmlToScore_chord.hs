module Score.XmlToScore_chord where
import Data.Map (Map)
import Common
import Score.ScoreData
import qualified Data.Set as S
import Data.Maybe
import Data.Set(Set)
import qualified Data.Map as M
import XmlDoc.XmlDocData
import Debug.Trace

prelimChordsToChords :: Map Loc [MarkD] -> Map Loc (Map Int PrelimChord) -> 
    Map Loc (Map Int Chord)
prelimChordsToChords = error "foo"

oneChord :: Map Loc (Map Int PrelimChord) -> Map Loc [MarkD] -> (Loc,(Int,PrelimChord)) -> Maybe Chord
oneChord chords marks (begLoc,(vn,p@(PrelimChord endLoc mods notes type_ graces)))
    | isJust isTrillResult       = Just $ oneChordTrillCase isTrillResult begLoc vn p
    | isJust doubTremStartResult = oneChordDoubTremCase chords begLoc vn p
    | isJust doubTremStopResult  = Nothing
    | otherwise                  = Just $ oneChordMainCase begLoc vn p
    where
        isTrillResult :: Maybe Int
        isTrillResult = isTrill marks begLoc vn
        doubTremStartResult :: Maybe Int
        doubTremStartResult = findDoubTremStart mods
        doubTremStopResult :: Maybe Int
        doubTremStopResult = findDoubTremStop mods


oneChordDoubTremCase :: Map Loc (Map Int PrelimChord) -> Loc -> Int -> PrelimChord -> Maybe Chord
oneChordDoubTremCase chords beg vn ch = error "foo"


oneChordTrillCase :: Maybe Int -> Loc -> Int -> PrelimChord -> Chord
oneChordTrillCase loc vn ch = error "foo"


oneChordMainCase :: Loc -> Int -> PrelimChord -> Chord
oneChordMainCase beg vn (PrelimChord end mods notes grType graces) = error "foo"


tNoteToNote :: TNote -> Note
tNoteToNote tn = Note (tnPitch tn) (tnTieStart tn) (tnEnd tn) (mNotehead $ tnNotehead tn)


mNotehead :: Maybe XNotehead -> Notehead
mNotehead (Just n) = "warning: hardcoded normal head" `trace` NormalHead
mNotehead Nothing  = NormalHead



-- data Notes = NSingles (Map Int Note)
--            | NTrill TrillTremNote (Map Int Note) (Map Int Note)
--              -- Bool is trill diff flag: Nothing for tremolo, Just x
--              -- for a trill with an upper note x steps above the base note
--              deriving(Eq,Ord,Show,NFData,Generic)

-- data Chord = Chord
--   { cEndLoc        :: Loc
--   , cModifiers     :: Set ChordModifier
--   , cNotes         :: Notes
--   , cGraceType     :: Bool
--   , cGraces        :: [TNote]
--   }

-- data PrelimChord = PrelimChord
--   { prcEndLoc     :: Loc
--   , prcModifiers  :: Set ChordModifier
--   , prcNotes      :: [TNote]
--   , prcGraceType  :: Bool
--   , prcGraces     :: [TNote]
--   }



findDoubTremStart :: Set ChordModifier -> Maybe Int
findDoubTremStart mods = 
  listToMaybe [n | DoubTremStart n <- S.toList mods]


findDoubTremStop :: Set ChordModifier -> Maybe Int
findDoubTremStop mods = 
  listToMaybe [n | DoubTremStop n <- S.toList mods]

{-
              case isTrill symbols atLoc vn of
               Nothing  -> Just $ pc2c_main_case c1
               Just trN -> pc2c_trill_case atLoc trN c1
-}
isTrill :: Map Loc [MarkD] -> Loc -> Int -> Maybe Int
isTrill syms atLoc vn =
    M.lookup atLoc syms >>= listToMaybe . mapMaybe (mTrill vn)

mTrill :: Int -> MarkD -> Maybe Int
mTrill vn (SymbolMark s vTest) | vTest /= vn = Nothing
                               | otherwise = case s of
                                   "trill-natural" -> Just 0
                                   "trill-flat"    -> Just (-1)
                                   "trill-sharp"   -> Just 1
                                   "Trill"         -> Just 0
                                   _               -> Nothing

mTrill _ _ = Nothing

