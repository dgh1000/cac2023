module Cac.MusicXml where

import Text.XML.Light.Types
import Text.XML.Light.Output

data MyScorePart = MyScorePart
 { mspId :: String
 , mspName :: String
 , mspInstrName :: String
 }


data MxPart = MxPart
  { mxpId :: String
  , mxpMsrs :: [MxMsr]
  }


data MxMsr = MxMsr
  { mxmNum :: Int
  , mxmMsrAttr :: MxMsrAttr
  , mxmItems :: [MxMsrItem]
  }


data MxMsrAttr = MxMsrAttr
  { mxaDivisions :: Int
  , maxKey       :: Int   -- "fifths" -- not sure what that is
  , mxaTimeSig   :: MxTimeSig
  , mxaClef      :: MxClef
  }


data MxMsrItem = MmiNote 
  { mmiPitch     :: MxPitch
  , mmiDur       :: Int
  , mmiVoice     :: Int
  , mmiNoteType  :: MxNoteType
  }


data MxPitch = MxPitch
  { mxpStep :: Int
  , mxpOct  :: Int
  }
  
data MxClef = MxcTreble | MxcBass


data MxTimeSig = MxTimeSig Int Int


data MxNoteType = Mnt32 | Mnt16 | Mnt8 | Mnt4 | Mnt2 | Mnt1

simpleElem :: String -> [Attr] -> [Content] -> Element
simpleElem name attrs content = Element (simpleQ name) attrs content Nothing

  
simpleQ :: String -> QName
simpleQ s = QName s Nothing Nothing


simpleAttr :: String -> String -> Attr
simpleAttr name value = Attr (simpleQ name) value


simpleCData :: String -> CData
simpleCData s = CData CDataText s Nothing

simpleContent :: String -> Content
simpleContent s = Text $ simpleCData s
