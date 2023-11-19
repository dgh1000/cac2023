{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies,
             TypeSynonymInstances, FlexibleInstances #-}

module Instruments where

import Control.Arrow
import qualified Data.Map as M 
import Data.Map(Map)
import Data.Set(Set)
import Translation.Curves
import Translation
import Control.Lens.TH
import Score.ScoreData
import Common
import Common.CommonUtil
import Util.Showable
import Util.Map


type StaffCurves = Map String (String,Curve)

data Piano2 = Piano2
  { pno2Dests            :: Map String (Int,Int)
  , pno2AccentAmt        :: Double
  , pno2VCurveSingle     :: VelCurve
  , pno2VCurveTrill      :: VelCurve
  , pno2PedalChangeDelta :: Double
  , pno2Vol              :: Int
  , pno2AllLegato        :: Bool
  , pno2CtrlSettings     :: Map String [Int]
  }


-- NOTES 2021: 
-- initRaws: initialize certain values.. volume? expression?
-- miTrillModArtic: clearly looks at chord to determine name
--   of articulation which will probably translate to a table
--   different dests for different articulations... this sounds like
--   loading different channels but accessing them all from one articulation
--   makrer
-- miArticKs: keyswitches for articulations... why none?
--   velCurves: different for different articulations, yes that was 
--   perfectionism
-- aliases: shorter names to appear in score?
-- midDests: so we can send to expression as well as mod wheel? or
-- to multiple channels with loaded

data ModKsInstr = ModKsInstr
  { miInitRaws       :: [TrRaw]
  , miTrillModArtic  :: Set ChordModifier -> Maybe Int -> Maybe String
  -- <chord mods> <trill size, if any> <maybe specific artic>
  , miArticDests     :: Map String (Int,Int)
  , miArticKs        :: Map String (Maybe Int)
  , miArticVelCurves :: Map String VelCurve
  , miArticAliases   :: [(String,String)]
  , miModDests       :: [(Int,Int)]
  , miModVelCurve    :: VelCurve
  }

data NoteConf = NoteConf
  { _noteConfSNote :: SNote
  , _noteConfPatch :: String
  }

makeFields ''NoteConf

-- NOTES 2023: "Any" seems to abstract the meaning of an instrument
--   down to manipulation of SNotes based on Marks and probably also
--   based on information contained in an SNote as well as a mod
--   function based on staff name and range of Locs . Does it have
--   to be mod that it generates? How does any simple generate 
--   a vol curve?
data Any = Any
  { _anyNotesFn :: Map Loc [MarkD] -> SNote -> Tr SNote
  , _anyModFn   :: String -> Loc -> Loc -> Tr [TrRaw]
  }

makeFields ''Any

--------- Hoopus (Hollywood Orchestra Opus Edition)


-- data HoopusCtrl11Config = HC11CNotUsed | HC11CVolume Int Int

data HoopusNoteDescr = HoopusNoteDescr
  { _hoopusNoteDescrMarkedArtic     :: String
  , _hoopusNoteDescrModifiers       :: Set ChordModifier
  , _hoopusNoteDescrSlurNext        :: Bool
  , _hoopusNoteDescrIsTrill         :: Int -- 0 means no trill, 1 HT, 2 WT
  , _hoopusNoteDescrIsTrem          :: Bool

  }

makeFields ''HoopusNoteDescr

data HoopusModConfig = HMCContinuousOnly | HMCSelector (HoopusNoteDescr -> SNote -> Int)

data HoopusVelConfig = HVCLoudness VelCurve | HVCFunc (HoopusNoteDescr -> SNote -> Int)



data HoopusChan = HoopusChan
  { _hoopusChanMidiChan           :: Int
  , _hoopusChanModConfig          :: HoopusModConfig
  -- , _hoopusChanCtrl11Config       :: HoopusCtrl11Config
  , _hoopusChanVelConfig          :: HoopusVelConfig
  , _hoopusChanShortenForStaccato :: Bool
  , _hoopusChanAccentAmt          :: Double
  -- , _hoopusArticDoMod             :: SNote -> Maybe Int
  }

makeFields ''HoopusChan

data Hoopus = Hoopus
  { -- something about the name 'stream' was causing an error
    _hoopusStreamm            :: Int
  , _hoopusStaffName          :: String
  , _hoopusDetermineChan      :: HoopusNoteDescr -> SNote -> HoopusChan
  , _hoopusModLoudnessDests   :: [(Int,(Int,Int))]
  , _hoopusExprLoudnessDests  :: [(Int,(Int,Int))]
  , _hoopusSepSame            :: Bool
  }

makeFields ''Hoopus




----------------------------------------------------------------------
-- examples of BracketFuncs
--
--   What does a BracketFunc do?
--
-- type BracketFunc = MetaInstr -> Synth -> BracketParams ->
--                   Tr ([TrRaw],[SNote])
--
--     Input:
--
--       MetaInstr data, and Synth data
--
--       the notes, in the form of BracketParams data. this contains the
--       bracket name, the staff name, beginning and end locs, and the notes
--       themselves
--
--     Output
--
--       list of TrRaws that are needed:
--
--         (1) "bracket initializer raws" : used to initialize parameters of
--         software synth both specific to this bracket function and constant
--         throughout bracket phrase
--
--         (2) "control curve raws": repeated raws that adjust controls over
--         the duration of the phrase to provide shape in some way (changes to
--         the mod controller fall in this category)
--
--       list of SNotes, directed toward specific channel(s) and with
--       keyswitches or note-associated raw values present
--



data BracketParams = BracketParams
  { _bracketParamsName       :: String
  , _bracketParamsStaffN     :: String
  , _bracketParamsBegLoc     :: Loc 
  , _bracketParamsEndLoc     :: Loc
  , _bracketParamsCenterLoc  :: Maybe Loc 
  , _bracketParamsNotes      :: [SNote]
  , _bracketParamsScoreParam :: [Double]
  }


makeFields ''BracketParams

-- when do we add synth control sessions. x y z
 
data SimpleBracketFunc = SimpleBracketFunc
  { _simpleBracketFuncDest          :: (Int,Int)
  , _simpleBracketFuncCtrlSettings  :: [(Int, Int)]
  , _simpleBracketFuncVelCurve      :: VelCurve
  , _simpleBracketFuncLoudCtrl      :: Maybe (Int,VelCurve)
  }


makeFields ''SimpleBracketFunc

type BracketFunc = MetaInstr -> Synth -> BracketParams -> Tr ([TrRaw],[SNote])

data Synth = Synth
  { -- Accent amount.
    _synthAccent            :: Double
  , _synthBracketFuncs      :: Map String BracketFunc
  , _synthNamedCtrlSettings :: Map String [(Int,Int)]
  }

makeFields ''Synth



--------------------------------------------------------------------
--                  synth helper data (some debugging)


data StaffLocTagged a = StaffLocTagged String Loc a


data SynthArticBracket = SabBrackR String | SabBrackL String | SabArtic String
  deriving(Eq,Ord,Show)


sltToTuple :: StaffLocTagged b -> (Loc, b, String)
sltToTuple (StaffLocTagged s l x) = (l,x,s)


toSltSL :: Map String (Map Loc [a]) -> [StaffLocTagged a]
toSltSL m = concatMap g $ M.toAscList m
    where
      g (staffN,m2) = concatMap h $ M.toAscList m2 
        where
          h (loc,xs) = map (StaffLocTagged staffN loc) xs


toSltLS :: Map Loc (Map String [a]) -> [StaffLocTagged a]
toSltLS = toSltSL . flipMap
  


instance Eq a => Eq (StaffLocTagged a) where
  (==) x y = sltToTuple x == sltToTuple y
  

instance Ord a => Ord (StaffLocTagged a) where
  compare x y = compare (sltToTuple x) (sltToTuple y)


instance ShowItemClass a => ShowItemClass (StaffLocTagged a) where
  showI (StaffLocTagged staffN loc x) = Component (staffN ++ showLoc2 loc)
    True [showI x]


data MapStaffLoc a = MapStaffLoc (Map String (Map Loc a))


mslFromTuples :: [(String,(Loc,a))] -> MapStaffLoc a
mslFromTuples ts = MapStaffLoc $ M.map M.fromList m1
  where
    --  m1 :: Map String [(Loc,a)]
    m1 = M.fromList $ map (second (:[])) ts



tdMapLookup :: Ord k => k -> Map k a -> a
tdMapLookup k m = case M.lookup k m of {Just x -> x}
