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

tNotesToPrelimChords :: Map Loc (Map Int [TNote]) -> Map Loc (Map Int PrelimChord) 
tNotesToPrelimChords = M.map (M.map tNotesToChords2)

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

tNotesToChords2 :: [TNote] -> PrelimChord
tNotesToChords2 tns = 
    PrelimChord endLoc modifiers reg type_ gracesSorted
    where
        (graces,reg) = tNotesSeparateGraces tns
        (type_,gracesSorted) = case graces of
            [] -> (False,[])
            xs -> (graceType xs,sortGraces xs)
        modifiers :: Set ChordModifier
        modifiers = S.fromList $ concatMap (getChordModifiers . tnNotations) reg
        endLoc = case (reg,graces) of
          (x:_,_) -> tnOrigEnd x
          (_,x:_) -> tnOrigEnd x
          _       -> error "foo"

-- tNotesSeparateGraces
--
-- output :: (<graces>,<non-graces>)
tNotesSeparateGraces :: [TNote] -> ([TNote],[TNote])
tNotesSeparateGraces = L.partition (isJust . tnIsGrace)

{-
tNotesToNoteMap :: [TNote] -> Map Int Note
tNotesToNoteMap tns = M.fromList notes
  where
    notes :: [(Int,Note)]
    notes = map g tns
    g :: TNote -> (Int,Note)
    g t = (midiPitch $ tnPitch t,tNoteToNote t)
-}

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

