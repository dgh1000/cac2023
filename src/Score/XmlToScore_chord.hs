{-# LANGUAGE TupleSections #-}
module Score.XmlToScore_chord where
import Data.Map (Map)
import Common
import Score.ScoreData
import qualified Data.Set as S
import Control.Arrow
import Data.Maybe
import Data.Set(Set)
import qualified Data.Map as M
import XmlDoc.XmlDocData
import Debug.Trace
import Text.Printf
import Common.CommonUtil
import Util.Exception

prelimChordsToChords :: Map Loc [MarkD] -> Map Loc (Map Int PrelimChord) -> 
    Map Loc (Map Int Chord)
prelimChordsToChords marks chords = error "foo"
  where
    g :: (a,[(b,c)]) -> [(a,(b,c))]
    g (a,xs) = map (a,) xs
    h :: [(a,[(b,c)])] -> [(a,(b,c))]
    h = concatMap g
    v1 :: Map Loc [(Int,PrelimChord)]
    v1 = M.map M.toAscList chords
    v2 :: [(Loc,[(Int,PrelimChord)])]
    v2 = M.toAscList v1
    v3 :: [(Loc,(Int,PrelimChord))]
    v3 = h v2
    v4 :: [(Loc,(Int,Chord))]
    v4 = mapMaybe (oneChord chords marks) v3
    v5 :: [(Loc,[(Int,Chord)])]
    v5 = 

    -- [(Loc,(Int,PrelimChord))]

oneChord :: Map Loc (Map Int PrelimChord) -> Map Loc [MarkD] -> 
            (Loc,(Int,PrelimChord)) -> Maybe (Loc,(Int,Chord))
oneChord chords marks (begLoc,(vn,p@(PrelimChord endLoc mods notes type_ graces)))
    | isJust isTrillResult       = 
        Just $ oneChordTrillCase (fromMaybe 0 isTrillResult) begLoc vn p
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


oneChordDoubTremCase :: Map Loc (Map Int PrelimChord) -> Loc -> Int -> 
                        PrelimChord -> Maybe (Loc,(Int,Chord))
oneChordDoubTremCase 
  chords 
  beg 
  vn 
  before@(PrelimChord end mods notes _ _) = out
  where
    mAfter :: Maybe PrelimChord
    mAfter = M.lookup end chords >>= M.lookup vn
    out = case mAfter of
      Nothing -> printf ("warning, no chord follows doub. tremolo " ++ 
        "at %s") (showLoc2 end) `trace` Nothing
      Just after -> Just $ doubTremCase beg vn before after


doubTremCase :: Loc -> Int -> PrelimChord -> PrelimChord -> (Loc,(Int,Chord))
doubTremCase beg vn ch1 ch2 = (beg,(vn,out))
  where
    notes1 :: [Note]
    notes1 = map tNoteToNote $ prcNotes ch1
    notes2 :: [Note]
    notes2 = map tNoteToNote $ prcNotes ch2
    map1 :: Map Int Note
    map1 = M.fromList $ map (\n -> ((midiPitch . nPitch) n, n)) notes1
    map2 :: Map Int Note
    map2 = M.fromList $ map (\n -> ((midiPitch . nPitch) n, n)) notes2
    out = Chord (prcEndLoc ch2) (prcModifiers ch1) 
      (NTrill TtnTremolo map1 map2) []


oneChordTrillCase :: Int -> Loc -> Int -> PrelimChord -> (Loc,(Int,Chord))
oneChordTrillCase upperAlter loc vn ch =
  let msg = printf ("something wrong; a trill chord has more than " ++
                    "one note at %s") (showLoc2 loc)
      note1 :: Note
      note1 = case prcNotes ch of
        [x] -> tNoteToNote x
        _   -> throwMine msg
      pitch1 = nPitch note1
      midi1 = midiPitch pitch1
      (midi2,pitch2) = computeTrillPitch upperAlter pitch1
      trillDiff = midi2-midi1
      note2 = note1 {nPitch=pitch2}
      new1 = M.fromList [(midi1,note1)]
      new2 = M.fromList [(midi2,note2)]
  in (loc,(vn, 
        Chord (prcEndLoc ch) (prcModifiers ch) 
          (NTrill (TtnTrill trillDiff) new1 new2) []))



oneChordMainCase :: Loc -> Int -> PrelimChord -> (Loc,(Int,Chord))
oneChordMainCase beg vn (PrelimChord end mods notes grType graces) = 
  (beg,(vn,Chord end mods (NSingles $ M.fromList notesList) graces))
  where
    notesList :: [(Int,Note)]
    notesList = map ((\n -> (midiPitch . nPitch $ n,n)) . tNoteToNote) notes

-- data Notes = NSingles (Map Int Note)
--            | NTrill TrillTremNote (Map Int Note) (Map Int Note)
--              -- Bool is trill diff flag: Nothing for tremolo, Just x
--              -- for a trill with an upper note x steps above the base note
--              deriving(Eq,Ord,Show,NFData,Generic)

computeTrillPitch :: Int -> Pitch -> (Int,Pitch)
computeTrillPitch upperAlter (Pitch _ step1 alter1 octave1) = (newMidi,pitch2)
  where
    step2 = (step1+1) `mod` 7
    octave2 = if step1 == 6 then octave1+1 else octave1
    newMidi = stepAlterOctToMidi step2 upperAlter octave2
    pitch2 = Pitch newMidi step2 upperAlter octave2


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

