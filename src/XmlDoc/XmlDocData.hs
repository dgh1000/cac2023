{-# LANGUAGE DeriveAnyClass,DeriveGeneric #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module XmlDoc.XmlDocData where

import Data.Map(Map) 
import Common
import GHC.Generics hiding(Meta)
import Control.DeepSeq


data XScore = XScore
  { xPartInfos :: Map String XPartInfo -- map of id to part human-readable name
  , xParts :: Map String XPart   
  , xIsSib  :: Bool      
  }

data XPartInfo = XPartInfo
  { xpiName :: String -- the <name> element, normally a sensible human-
                      -- understandable name; e.g. "Violin"
  }

data XPart = XPart
  { xpMsrs :: [XMsr]
  }

data XMsr = XMsr 
  { xmMsrNum :: Int
  , xmMsrAttr :: Maybe XMsrAttr
  , xmMsrDatas :: [XMsrData]
  }


data XMsrAttr = XMsrAttr
  { xaDpq :: Maybe Int       -- divisions per quarter
  , xaNumer :: Maybe Int
  , xaDenom :: Maybe Int
  }

-- does every 
data XMsrData = 
    XMDDirection XDirection (Maybe Int) (Maybe Int) (Maybe Int) Int
       -- <direction> <maybe offset> <maybe voice> <maybe staff> <order>
  | XMDNote XNote Int -- xnote, order
  | XMDBackup Int Int -- order
  | XMDForward Int Int -- order
  | XMDOther String Int -- element name, order
  deriving(Show)

orderOf :: XMsrData -> Int
orderOf (XMDNote _ i) = i


--                      Sibelius and the grand staff
--
--           (regarding voice numbers of Directions and Notes)
-- 
-- Note that Sibelius alters the voice numbers of Notes on lower staff. So
-- the question here is whether it does that with Directions also. The answer
-- is, with some and not others.
--
-- Metronome markings: 
--
--   These don't have a voice in Sibelius' internal representation. In the
--   example it exported them as staff 1, voice 1.
-- 
-- Words.
--
--   System text like rit. and accel. was exported with no voice given.
--
--
--   Technique and Expression text:
--
--      Did alter them to match lowest note voice on each staff, regardless
--      of which voice they were attached to.
--
-- Dynamics
--
--   Do match the notes
--
-- Wedge and Pedal
--
--   In one case a <wedge> did not have a voice.
--
--   In other cases Sibelius did not alter them to match the notes. Instead
--   they matched the orginal voices. When processing them
--
-- Custom trill symbols in XDOtherDirection
--
--   Did alter them to match lowest note voice on each staff, regardless of
--   which voice they were attached to (I think that trills are actually
--   considered by Sibelius to be attached to the staff rather to a specific
--   voice)
-- 

data XDirection = 
    XDMetronome String Int      -- beat-unit, per-minute
 |  XDWords String (Maybe Int) -- text string, and default-y attribute
                                       -- and font
 |  XDDynamics String           -- string is "pp", "ff" etc, "fp"
 |  XDWedge WedgeType           -- <type> <staff number>       
 |  XDPedal PedalEvt Bool       -- bool is true if the attribute 'line' is 'yes'
 |  XDOctaveShift Int           -- int is number of octaves up (negative 
                                --   means down). 0 means a 'stop'
 |  XDOtherDirection String -- where sibelius puts my custom trill symbols
 deriving(Show)

data XNote = 
   XNRest
     { xnDuration :: Int
     , xnChord :: Bool
     , xnVoice :: Maybe Int
     , xnStaff :: Maybe Int
     , xnNotations :: [XNotation]
     , xnOrder :: Int }
 | XNNote
     { xnDuration :: Int
     , xnIsGrace :: Maybe Bool -- True means 'slash' (acciaccatura, before the beat)
     --  , xnGraceOrder :: Int
     , xnChord :: Bool
     , xnVoice :: Maybe Int
     , xnStaff :: Maybe Int
     , xnPitch :: XPitch
     , xnTieStart :: Bool
     , xnTieStop :: Bool
     , xnNotations :: [XNotation]
     , xnNotehead :: Maybe XNotehead
     , xnOrder :: Int }
  deriving(Show)

data XNotehead = XNotehead
  { xnhType :: String }
             deriving(Eq,Ord,Show,NFData,Generic)

data XNotation =
 --  XNTied String         -- string is type, "start" or "stop"
   XNSlur String (Maybe Int) -- <"start" or "stop">, <number>
 | XNArticulations [XArticulation]
 | XNOrnaments [XOrnament]
 | XNFermata
 | XNArpeggiate
 | XNTechnical [XTechnical]
 deriving(Show,Eq,Ord,NFData,Generic)

data XArticulation =
   XAStaccato
 | XAStaccatissimo
 | XAAccent
 | XAStrongAccent
 | XATenuto
 | XADetachedLegato
 deriving(Show,Eq,Ord,NFData,Generic)

data XOrnament =
   TrillMark
 | Tremolo TremoloType Int    -- number of bars
 deriving(Show,Eq,Ord,NFData,Generic)

data XTechnical = XTOpenString
                | XTDownBow
                | XTUpBow
                deriving(Show,Eq,Ord,NFData,Generic)

data TremoloType = TremoloSingle
                 | TremoloStart
                 | TremoloStop
  deriving(Show,Eq,Ord,NFData,Generic)

data XPitch = XPitch 
  { xStep :: String 
  , xAlter :: Int 
  , xOctave :: Int }
            deriving(Show)

data WedgeType = WedgeCresc | WedgeDim | WedgeStop
               deriving(Show)

----------------------------------------------------------------------
----------------------------------------------------------------------
--       IXmlDoc - intermediate between XmlDoc and PerfDoc

{-
data IXmlDoc = IXmlDoc 
  { ixMsrInfos :: Map Int IXMsrInfo
  , ixParts :: Map String IXPart
  }
-}

data IXMsrInfo = IXMsrInfo
  { ixDpq   :: Int    -- divisions per quarter
  , ixNumer :: Int
  , ixDenom :: Int }

{-
data IXPart = IXPart
  { ixMsrDatas :: Map Loc [XMsrData]
  , ixPartNotes :: Map Loc [XNote] }
-}

----------------------------------------------------------------------
----------------------------------------------------------------------
data TieEnd = TieEnd
  { tePitch :: Pitch
  , teVoice :: Int
  , teStaff :: Maybe Int
  , teEnd   :: Loc
  }

data TNote = TNote
  { tnPitch    :: Pitch
  , tnVoice    :: Int
  , tnStaff    :: Maybe Int
  , tnTieStart :: Bool
  , tnTieStop  :: Bool
  , tnBegin    :: Loc
  , tnEnd      :: Loc
  , tnOrder     :: Int  -- index into the order this note appeared 
                        -- in the XMsr
  , tnNotations :: [XNotation]
  , tnNotehead  :: Maybe XNotehead
  , tnIsGrace   :: Maybe Bool
  }
             deriving(Eq,Ord,Show,NFData,Generic)



data TestPart = TestPart (Map Loc [XMsrData])
