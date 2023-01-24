
{-# LANGUAGE DeriveAnyClass,DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Score.ScoreData where

import qualified Data.Array as A
import qualified Data.Set as S
import qualified Data.Map as M
import qualified Data.List as L
import qualified Common as CD
import Debug.Trace
import GHC.Generics hiding(Meta)
import Control.DeepSeq
import Control.Arrow
import Data.Array(Array)
import Data.Set(Set)
import Data.Map(Map)
import Common
import Util.Exception
import XmlDoc.XmlDocData

{-
data ScoreObject = SoMarks { soLoc       :: Loc
                           , soStaffName :: String
                           , soMarks     :: [Mark Double]
                           }
                 | SoCk    { soLoc       :: Loc
                           , soStaffName :: String
                           , soVn        :: Int
                           }
                 deriving(Eq)


instance Ord ScoreObject where
  compare x y | soLoc x < soLoc y = LT
              | soLoc x > soLoc y = GT
              | otherwise = case (x,y) of
                  (SoMarks{},SoCk{}) -> LT
                  (SoCk{},SoMarks{}) -> GT
                  otherwise          -> EQ
                         
-}
data Score = Score
  { scTimeSigs       :: Map Int TimeSig
  , scMarks          :: Map Loc (Map String [MarkD])

  -- cached data related to Marks
  , scMarksByStaff   :: Map String (Map Loc [MarkD])
  , scMarkers        :: Map String Markers

  , scStaves         :: Map String Staff
  -- , scUsedMsrs       :: Array Int Bool
  , scUsedMsrs       :: Set Int
  }


emptyScore = Score M.empty M.empty M.empty M.empty M.empty
             S.empty
             -- (A.listArray (1,1) [True])


--
-- data Grace = Acciaccatura Int -- on the bea
--            | Appogiatura Int

-- data AcciSeq = AcciSeq [Int]
-- data AppoSeq = AppoSeq [Int]

data GraceNote = GraceNote
  { gnMidiPitch :: Int
  , gnVoice     :: Int
  , gnTieStart  :: Bool
  , gnType      :: Bool -- True = acciaccatura, False = appoggiatura
  }

-- Bool: True = acci, False = appo
data GraceNoteSeq = GraceNoteSeq Bool [GraceNote]

data Staff = Staff
  { stName         :: String
  , stDynamics     :: [Map Loc Dynamic]
  , stHairpins     :: [Map Loc Hairpin]
  , stPedalEvts    :: Map Loc PedalEvt
  , stMetSymMarks  :: Map Loc [MarkD]
  , stMaxTrueEnd   :: Loc
  , stUsedMsrs     :: Set Int
  , stSlurs        :: Map Loc Loc
  , stChords       :: Map Loc (Map Int Chord)
  , stGrace        :: Map Loc (Map Int GraceNoteSeq)
  -- map of voice number to staff number it resides on
  , stVoiceToStaff :: Map Int Int
  , stStaffToVoice :: Map Int [Int]
  }


data MacroDefn = MacroDefn String Int [MacroDefnChar]

data MacroDefnChar = MdcChar Char
                   | MdcArg Int

data MacroInstance = MacroInstance String [String]
  -- name of macro to apply, arguments

data Pass1Word = P1MacroDefn MacroDefn
               | P1MacroInstance MacroInstance
               | P1Normal String
                 -- ^ <maybe name of macro defn that was applied> <text>
               | P1Comment



data PointType = PtW | PtCrescDescr | PtDescrCresc


data WarpSide = LeftWarp | RightWarp
                deriving(Show,Eq,Ord)

type Variables = Map Loc (Map String Double)



data RawMark a = SetVar String NumVar
               | Comment
               | RmMark (Mark a)

type MarkN = Mark NumVar

type MarkD = Mark Double

data Mark a = SymbolMark String Int
  -- SetTempo <maybe pre-tempo> <post-tempo or only-tempo> <flag: * present>
            | SetTempo (Maybe a) a Bool
            | ArpDelta Double
            | StacDur Double
            | Trunc Double
            | LTrunc Double
            | Extend Double
            | AbsWarp WarpSide a
            | Pause a
            | PostPause a
            | W
            | RampBeg a
            | RampEndBeg a a
            | RampEnd a
            | RitAccel
            | TrillShapeMark TrillShape
            | TremShapeMark TrillShape
            | Artic String
            | Patch String
            | BracketL String (Maybe String) [a]
            | BracketR String
            | BracketC
            | Boundary2 (Maybe a)
            | Adjust2 Bool Double a
            | MidiCtrl Int a Int -- <ctrl num> <value as numvar> <value as Int>
            | CtrlSetting String
            | GenericShape (GsContent a)
            -- | ControlShape (CtrlShapeContent a)
            | Lev1DynL   Double
            | Lev1DynR   Double
            | Lev1DynLR  Double Double
            | Lev2DynL Double a
            | Lev2DynR Double a
            | Lev2DynLR Double a
            | TimeShift (Maybe a) a Bool
            | SpliceBeg String
            | SpliceEnd String
                -- time shift amount as fraction of level 1 tempo at this
                -- point. ramp flag.
            deriving(Show)


data GsContent a = GsOneLoc String [a]
                 | GsLeft   String [a]
                 | GsCenter String                  | GsRight  String
                 deriving(Show,Eq,Ord)


data CtrlShapeContent a = CscLeft String [a]
                        | CscCenter String
                        | CscRight String
                        deriving(Show,Eq,Ord)


data TempoModify = TmRamp      Double Double
                 | TmRampParab Double Double Bool
                   -- parabolic ramp shape. bool = true means steepest slope
                   -- at beginning, otherwise steepest slope at end
                   deriving(Show,Eq,Ord)

data WordDirection = WdBelow String | WdAbove String

data Markers = Markers
  { maWs :: Set Loc
  , maCD :: Set Loc
  , maDC :: Set Loc
  , maCaret :: Set Loc
  }


data NumVar = NumVar Double (Maybe String)
                deriving(Show,Eq,Ord)


data TrillShape = TrillShape TrillStep [(Double,Int)] TrillStep
                  deriving(Show,Eq,Ord)


data TrillStep = Upper | Lower
                 deriving(Show,Eq,Ord)


data Chord = Chord
  { cEndLoc        :: Loc
  , cModifiers     :: Set ChordModifier
  , cNotes         :: Notes
  , cGraceType     :: Bool
  , cGraces        :: [TNote]
  }
           deriving(Eq,Ord,Show,NFData,Generic)

-- data ChordContent = SinglesContent (Map Int Note)
--                  | TrillContent Bool Bool (Set Int)
                    -- True: trill, Fal:trem
                    -- True: start on upper, False: start on lower

-- okay we need

-- how do you identify this with a NoteKey? Just an Int!


data TrillTremNote = TtnTremolo
                   | TtnTrill Int
                   deriving (Eq,Ord,Show,NFData,Generic)


data Notes = NSingles (Map Int Note)
           | NTrill TrillTremNote (Map Int Note) (Map Int Note)
             -- Bool is trill diff flag: Nothing for tremolo, Just x
             -- for a trill with an upper note x steps above the base note
             deriving(Eq,Ord,Show,NFData,Generic)

data GNote = GNote
  { gnPitch   :: Pitch
  , gnIsTied  :: Bool
  , gnTrueEnd :: Loc
  }
    deriving (Eq,Ord,Show,NFData,Generic)

data InterNote = InterNote
  { inEnd       :: Loc
  , inPitch     :: Pitch
  , inIsGrace   :: Maybe Bool
  , inVoice     :: Int
  , inTieStart  :: Bool
  , inTieStop   :: Bool
  , inNotations :: [XNotation]
  , inNotehead  :: Notehead }

-- try to figure this out 9/1/22. prelim chord. major feature seems 
-- to be end loc. what's in a Note? how is this prelim and what is different
-- about final chord?
-- 
data PrelimChord = PrelimChord
  { prcEndLoc     :: Loc
  , prcModifiers  :: Set ChordModifier
  , prcNotes      :: [Note]
  }


-- Note has pitch, tied, true end (is that after ties), notehead. is this
-- missing anything? voice? 
data Note = Note
  { nPitch    :: Pitch
  , nIsTied   :: Bool
  , nTrueEnd  :: Loc
  , nNotehead :: Notehead
  }
          deriving(Eq,Ord,Show,NFData,Generic)


data ChordKey = ChordKey
  { ckStaffName   :: String
  , ckChordLoc    :: Loc
  , ckVoiceNum    :: Int
  , ckContent     :: Either [Int] ([Int],[Int])
  }
              deriving(Eq,Ord,Show)


notesToContent :: Notes -> Either [Int] ([Int],[Int])
notesToContent (NSingles m) = Left $ M.keys m
notesToContent (NTrill _ m1 m2) = Right (M.keys m1,M.keys m2)


data NoteKey = NoteKey ChordKey Int
             deriving(Eq,Ord,Show)


-- Loc, voice number, pitch
data StaffNoteKey = StaffNoteKey Loc Int Int

data ChordModifier = SingTrem Int
                   | DoubTremStart Int
                   | DoubTremStop Int
                   | Arpeggiate
                   | Staccato
                   | Staccatissimo
                   | Tenuto
                   | Accent
                   | StrongAccent
                   | Marcato
                   | Fermata
                   | DownBow
                   | UpBow
                   | OpenString
                     deriving(Show,Eq,Ord,NFData,Generic)


data Hairpin = Hairpin
  { hType :: HairpinType
  , hEnd  :: Loc
  , hStaffNum :: Int
  }
             deriving(Show)

data HairpinType = Crescendo | Diminuendo
                 deriving(Show,Eq)

data Text = TechniqueText String
          | ExpressionText String
          deriving(Show)



data OctaveLine = OctaveLine Int Loc   -- <# octaves shift>, <end loc>


chordTrueEnds :: Chord -> [(Int,Loc)]
chordTrueEnds (Chord endLoc _ notes _  _) = case notes of
  NSingles noteMap ->
    let f p = case M.lookup p noteMap of
          Just n -> nTrueEnd n
    in  map (id &&& f) $ M.keys noteMap
  NTrill _ s1 s2 ->
    let f s p = case M.lookup p s of
          Just n -> nTrueEnd n
    in map (id &&& f s1) (M.keys s1) ++ map (id &&& f s2) (M.keys s2)



mapOverNotes :: (NoteKey -> a) -> Score -> Map NoteKey a
mapOverNotes f sc =
  M.fromList . concatMap (mapOverNotes_staff f) . M.toList $ scStaves sc


mapOverNotes_staff :: (NoteKey -> a) -> (String,Staff) -> [(NoteKey,a)]
mapOverNotes_staff f (name,staff) =
  concatMap (mapOverNotes_loc f name) . M.toList $ stChords staff


mapOverNotes_loc :: (NoteKey -> a) -> String -> (Loc,Map Int Chord) ->
                    [(NoteKey,a)]
mapOverNotes_loc f staffName (loc,chords) =
  concatMap (mapOverNotes_vn f staffName loc) . M.toList $ chords

mapOverNotes_vn :: (NoteKey -> a) -> String -> Loc -> (Int,Chord) ->
                   [(NoteKey,a)]
mapOverNotes_vn f staffName loc (vn,chord) =
  map (\i -> (nk i,f $ nk i)) (pitchesOf $ cNotes chord)
  where

    contentOf (NSingles m) = Left $ M.keys m
    contentOf (NTrill _ xs ys) = Right (M.keys xs,M.keys ys)
    pitchesOf (NSingles m) = M.keys m
    pitchesOf (NTrill _ xs ys) = M.keys xs ++ M.keys ys
    nk i = NoteKey (ChordKey staffName loc vn (contentOf $ cNotes chord)) i


{-

allNoteKeys :: Score -> [NoteKey]
allNoteKeys sc = concatMap allNKs_staff . M.toList $ scStaves sc

allCKs_staff :: (String,Staff) -> [ChordKeys]
allCKs_staff (name,staff) =
  concatMap (allNKs_loc name) . M.toList $ stChords staff

allNKs_loc :: String -> (Loc,Map Int Chord) -> [NoteKey]
allNKs_loc 

-}



