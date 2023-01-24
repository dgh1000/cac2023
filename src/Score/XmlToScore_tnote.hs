-- Purpose of this module is functions that covert [TNote]
module Score.XmlToScore_tnote where
import Data.Set(Set)
import qualified Data.Set as S
import Data.Map(Map)
import qualified Data.Map as M
import Data.Maybe
import qualified XmlDoc.XmlDocData as XD
import qualified Data.List as L
import Common
import XmlDoc.XmlDocData
import Score.ScoreData
import Debug.Trace ( trace )
import Data.Function ( on )


tNotesToChords :: Map Loc [MarkD] -> Map Loc (Map Int [TNote]) -> Map Loc (Map Int Chord) 
tNotesToChords m = error "foo"

-- data Chord = Chord
--   { cEndLoc        :: Loc
--   , cModifiers     :: Set ChordModifier
--   , cNotes         :: Notes
--   , cGraceType     :: Bool
--   , cGraces        :: [TNote]
--   }
-- data Notes = NSingles (Map Int Note)
--            | NTrill TrillTremNote (Map Int Note) (Map Int Note)
--              -- Bool is trill diff flag: Nothing for tremolo, Just x
--              -- for a trill with an upper note x steps above the base note
--              deriving(Eq,Ord,Show,NFData,Generic)

tNotesToChords2 :: [TNote] -> Chord
tNotesToChords2 tns = 
    Chord endLoc modifiers (NSingles $ tNotesToNoteMap reg) type_ gracesSorted
    where
        (graces,reg) = tNotesSeparateGraces tns
        (type_,gracesSorted) = case graces of
            [] -> (False,[])
            xs -> (graceType xs,sortGraces xs)
        modifiers :: Set ChordModifier
        modifiers = S.fromList $ concatMap (getChordModifiers . tnNotations) reg
        endLoc = earliestEndLoc reg

-- tNotesSeparateGraces
--
-- output :: (<graces>,<non-graces>)
tNotesSeparateGraces :: [TNote] -> ([TNote],[TNote])
tNotesSeparateGraces = L.partition (isJust . tnIsGrace)

tNotesToNoteMap :: [TNote] -> Map Int Note
tNotesToNoteMap tns = M.fromList notes
  where
    notes :: [(Int,Note)]
    notes = map g tns
    g :: TNote -> (Int,Note)
    g t = (midiPitch $ tnPitch t,tNoteToNote t)


sortGraces :: [TNote] -> [TNote]
sortGraces = L.sortBy (compare `on` tnOrder)

graceType :: [TNote] -> Bool
graceType tns = case tns of 
    [] -> error "foo"
    x:_ -> case tnIsGrace x of
        Nothing -> error "foo" 
        Just b  -> b

earliestEndLoc :: [TNote] -> Loc
earliestEndLoc tns = case map tnEnd tns of
    [] -> error "foo"
    xs -> minimum xs


tNoteToNote :: TNote -> Note
tNoteToNote tn = Note (tnPitch tn) (tnTieStart tn) (tnEnd tn) (mNotehead $ tnNotehead tn)


mNotehead :: Maybe XNotehead -> Notehead
mNotehead (Just n) = "warning: hardcoded normal head" `trace` NormalHead
mNotehead Nothing  = NormalHead


getChordModifiers :: [XNotation] -> [ChordModifier]
getChordModifiers = concatMap g 
  where
  g :: XNotation -> [ChordModifier]
  g (XNArticulations arts) = concatMap artToMod arts
  g (XNOrnaments orns)     = mapMaybe ornToMod orns
  g XNFermata              = [Fermata]
  g XNArpeggiate           = [Arpeggiate]
  g (XNTechnical techs)    = map techToMod techs
  g (XNSlur _ _)           = []
  artToMod XAStaccato       = [Staccato]
  artToMod XAStaccatissimo  = [Staccatissimo]
  artToMod XAAccent         = [Accent]
  artToMod XAStrongAccent   = [StrongAccent]
  artToMod XATenuto         = [Tenuto]
  artToMod XADetachedLegato = [Staccato,Tenuto]
  ornToMod (Tremolo type_ nBars) = Just $ case type_ of
    TremoloSingle  -> SingTrem      nBars
    TremoloStart   -> DoubTremStart nBars
    TremoloStop    -> DoubTremStop  nBars
  ornToMod _ = Nothing
  techToMod XTOpenString = OpenString
  techToMod XTDownBow    = DownBow
  techToMod XTUpBow      = UpBow

-- data XNotehead = XNotehead
--   { xnhType :: String }
--   deriving(Show)

-- data Notehead = NormalHead
--               | DiamondHead
--                 deriving(Eq,Ord,Show,NFData,Generic)

-- data Note = Note
--   { nPitch    :: Pitch
--   , nIsTied   :: Bool
--   , nTrueEnd  :: Loc
--   , nNotehead :: Notehead
--   }
--           deriving(Eq,Ord,Show,NFData,Generic)

-- data TNote = TNote
--   { tnPitch    :: Pitch
--   , tnVoice    :: Int
--   , tnStaff    :: Maybe Int
--   , tnTieStart :: Bool
--   , tnTieStop  :: Bool
--   , tnBegin    :: Loc
--   , tnEnd      :: Loc
--   , tnOrder     :: Int  -- index into the order this note appeared 
--                         -- in the XMsr
--   , tnNotations :: [XNotation]
--   , tnNotehead  :: Maybe XNotehead
--   , tnIsGrace   :: Maybe Bool
--   }
