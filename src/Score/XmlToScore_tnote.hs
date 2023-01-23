-- Purpose of this module is functions that covert [TNote]
module Score.XmlToScore_tnote where
import Data.Map(Map)
import Common
import XmlDoc.XmlDocData
import Score.ScoreData
import Debug.Trace


tNotesToChords :: Map Loc [MarkD] -> Map Loc (Map Int [TNote]) -> Map Loc (Map Int Chord) 
tNotesToChords m = error "foo"


tNotesToChords2 :: [TNote] -> Chord
tNotesToChords2 tns = error "foo"


tNoteToNote :: TNote -> Note
tNoteToNote tn = Note (tnPitch tn) (tnTieStart tn) (tnEnd tn) (mNotehead $ tnNotehead tn)


mNotehead :: Maybe XNotehead -> Notehead
mNotehead (Just n) = "warning: hardcoded normal head" `trace` NormalHead
mNotehead Nothing  = NormalHead


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
