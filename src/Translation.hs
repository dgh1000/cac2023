{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveAnyClass ,
    DeriveGeneric, ExistentialQuantification, TemplateHaskell,
    MultiParamTypeClasses, FunctionalDependencies #-}

module Translation where

import Data.Map(Map)
import Score.ScoreData
import Common



import qualified Data.Map.Strict as M
import Text.Printf
import System.Random
import Data.Map.Strict(Map)
import Data.Maybe
import Data.Set(Set)
import GHC.Generics hiding(Meta)
import Control.Arrow
import Control.DeepSeq
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens
import Control.Lens.TH
import Common
import Score.ScoreData
import Util.Exception
import Util.Showable
import Util.Map
import Util.UtilData
import Common.CommonUtil




----------------------------------------------------------------------
--            FEB 2018      FEB 2018          FEB 2018
----------------------------------------------------------------------
--                   LARGER SHAPES
-- 
-- shapes in time
--
--   order in which time alterations are applied
--
--     level 1 tempo and ramps
--
--     level 2 ramps
--
--
--
--   level 1 tempo 
--
--
--     T=100, T=a : set level 1 tempo, to be flat until next tempo mark
--
--     T=100* T=a* , set level 1 tempo to ramp to next level 1 mark
--
--     80=T=100 the '80' is endpoint of previous ramp (if one), no need to put
--              *. and 100 is start of next ramp
--
--     80=T=100*
--
--
--   level 2 tempo
--
--     1=   =.3
--
--     1=   =.5=  =1
--
--     1=    =.5|1=      =.5
--
--   level 1 timeshift mark
--
--     shifts local staff forward or backward
--
--     %:3 :4%:5*
--
--     just % and numvar: ramp will be placed over prior, and end ramp
--     will be place at next time-shift mark
--
--     star means ramp to next timeshift
--
--     a plain number means multiply that by level 1 beat
--
--
--   dynamics
--
--     level 1:
--
--        specified by ppp, pp, p, mp, mf, f, ff, fff
--
--        also dynamic can have * following it to indicate ramp to next level
--        1 dynamic
--
--          p* fff*
--
--
--     level 2:
--
--       expresses delta from main dynamic
--
--       dynamic starts with - or + and has a numvar
--
--     hairpins
--
--       only make sense in a region with flat dynamic: add a ramp in an
--       otherwise flat region
--
--   how do = sign ramps fit in here?
--
--     level 2. basically applied after applying all level 1
--
--
--   generic shapes
--
--     can +/- be used to express delta with prevailing first level value?
--
--


-- common pattern: we have local and global time mods. we want to deal with
-- them as simply as possible. where we generate time mods we'd like to "dump"
-- them into similar containers so we can sort and process them.

-- ------------------------ NEW THOUGHTS ON EXPRESSION -------------------
--
-- I'd like timing expression that adjust relative timing of lower and higher
-- notes, based on note pitch rather than on the time map of the staff. this
-- will be some form of time adjustment. sort of like alter-end, but instead
-- altering the beginning.
--
-- Or should we adjust the time of the note when a time is looked up on the
-- timemap? can time maps have an associated map of offset based on pitch? we
-- need to know pitches when we create them. offset would be a function
--
-- we look up offset of a note based on average
--
-- offset would affect ending. advantage of putting it as specific offset
-- means it's available to us for adjustment or other algorithms when we
-- convert notes
--
-- 

type StaffMarkers = Map String Markers

-- 
data Context = Context [String] (Map Int TimeSig) StaffMarkers 

data Context2 = Context2 [String] (Map Int TimeSig) StaffMarkers

type ConMark = (String,(Loc,MarkD))  -- staff name, plus MarkD


data TempoLev1 = T1SetTempo (Maybe Double) Double Bool
               | T1RitAccel
               deriving(Eq)

data TempoLev2 = T2Ramp (Maybe Double) (Maybe Double)


data TempoMark =
    TmSetTempo 
  { tmOrigLoc :: Loc
  , tmDbl     :: Double
  }
    | TmRampBeg
  { tmOrigLoc :: Loc
  , tmDbl     :: Double
  }
  
    | TmRampEndBeg
  { tmOrigLoc :: Loc
  , tmDbl1    :: Double
  , tmDbl2    :: Double
  }
    | TmRampEnd
  { tmOrigLoc :: Loc
  , tmDbl     :: Double
  }
  | TmRitAccel
  { tmOrigLoc :: Loc }
  deriving(Show)
  
-- Questions August 2023: what is the difference between a TimeModMark and an UnitTimeMark?
data TimeModMark =
  TmmAbsWarp
  { tmmWarpSide  :: WarpSide
  , tmmDbl       :: Double
  }
     | TmmPause
  { tmmDbl       :: Double
  }
     | TmmAfterPause
  { tmmDbl       :: Double 
  }
     | TmmW
     | TmmBoundary
  { tmmMaybeDbl :: Maybe Double
  }
     | TmmAdjust
  { tmmWarpSide :: WarpSide
  , tmmDur      :: Double
  , tmmAmt      :: Double
  }  | TmmShift
  { tmmWarpSide :: WarpSide
  , tmmDur      :: Double
  , tmmAmt      :: Double    -- time shift in seconds
  }

  deriving(Show)


data GenericShapeMark =
  GsmLeft   { gsmOrigLoc :: Loc
            , gsmType    :: String
            , gsmAmts    :: [Double]
            }
  |
  GsmRight  { gsmOrigLoc :: Loc
            , gsmType    :: String
            }
  |
  GsmCenter { gsmOrigLoc :: Loc
            , gsmType    :: String
            }
  |
  GsmOneLoc { gsmOrigLoc :: Loc
            , gsmType    :: String
            , gsmAmts    :: [Double]
            }


data GsCombined = GsCombined
  { gcStaffN :: String
  , gcLoc    :: Loc
  , gcType   :: String
  , gcAmts   :: [Double]
  , gcCenter :: Maybe Loc
  , gcRight  :: Maybe Loc
  }
  deriving(Show)


data OneCurve = OneCurve (Map Double (Seg Double)) TimeSigs (Map Double Loc)
data Seg a = Seg Double a a  -- <end> <value1> <value2>
data Curve = Curve [OneCurve] 

data CtrlCurve = CtrlCurve (Int,Int) Int (Map Double (Seg Int))

---------------------------------------------------------------------

data ControlShapeMark =
  CsmLeft { csmOrigLoc  :: Loc
          , csmType     :: String
          , csmAmts     :: [Double]
          }


---------------------------------------------------------------------
--                 time-related

data RelTimeMap = RelTimeMap (Map Loc Double)

data AbsTimeMap = AbsTimeMap (Map Loc Double)

-- time mod useful with adjust v2, sort of like an absolute warp, but accel or
-- decel: by geometric sequence? how do we determine average of a geometric
-- series? probably integral of exponential curve would be estimate
--
-- modify UnitAbsWarp with a ramp of speed up, slow down, how about ratio of
-- each end


data Warp2Data = Warp2Data Loc Loc Double RampEndWhereOne



-- UnitWarp with the Either field as Left:
--
--    the first Loc is at the warp mark.
--
-- UnitAbsWarp:; the first Loc is at the warp mark
data UnitTimeMod = -- UnitRamp  Loc Loc Double Double
                   UnitPause Loc Rational
                 | UnitPostPause Loc Rational
                 | UnitWarp (Maybe String) (Either (Loc,Loc) (Loc,Loc,Loc))
                            Rational
                 | UnitAbsWarp Loc Loc Rational
                 | UnitWarp2 [Warp2Data]
                     -- <begin> <end> <amt change> <ratio1> <ratio2>
                 | Unit2Modify Loc Loc Loc TempoModify TempoModify
                               (Maybe Double)
                     -- <loc1> <loc2> <loc3> <tm1> <tm2> <normalize fraction>
                     --
                     -- Make two regions: loc1 to loc2, and loc2 to loc3. In
                     -- first region, modify tempo according <tm1>
                     -- instructions. in second region, modify according to
                     -- <tm2>. Then restore overall duration according to
                     -- <normalize fraction>. If this is 1, restore orginal
                     -- duration precisely. If this is something between 0 and
                     -- 1, take that fraction of the difference in tempo and
                     -- restore that amount.
                 | UnitAdjust (Maybe String) Loc Loc Loc Rational
                 | UnitRawRampWarp Loc Loc Rational RampEndWhereOne
                    -- warp by pitch. pitch of top of warp (the pitch in 
                    -- which there is no alteration in time), amount of warp in 
                    -- 10's of milliseconds per octave. 
                 | UnitWarpByPitch Loc Loc Double Double
                    -- This does a warp on either left side (Loc to -1 quarter) or
                    -- right side (Loc to +1 quarter) by the given relative change
                    -- in duration (Double). A negative Double means reduce the 
                    -- duration, positive means increase it.
                 | UnitLeftSideShift Loc Double
                 | UnitRightSideShift Loc Double


-- what kinds of unit warps do we need?
--
--   in general a warp lengthens or shortens time between two locs
--
--     can be ramped or not
--
--     new time can be expressed as delta to existing time, or a fixed
--     goal value
--
--  should there be a two-sided unit warp, used for the purposes of
--  balancing?
--
--    probably every mark that implies a need for balancing can be translated
--    to two unit warps

data Utm =
  UtmPause     { utmStaffN     :: Maybe String
               , utmLoc        :: Loc
               , utmAmt        :: Double
               }
  |
  UtmPostPause { utmStaffN     :: Maybe String
               , utmLoc        :: Loc
               , utmAmt        :: Double
               }
  |
  UtmWarp      { utmStaffN     :: Maybe String
               , utmLoc1       :: Loc
               , utmLoc2       :: Loc
               , utmRampShape  :: UtmRampShape
               , utmEAmt       :: Either Double Double
                  -- Left means relative change in time, Right means absolute
               }
  |
  UtmRamp     { utmStaffN      :: Maybe String
              , utmLoc1        :: Loc
              , utmLoc2        :: Loc
              , utmTempo1      :: Double
              , utmTempo2      :: Double
              }
  |
  UtmLShift   { utmStaffN      :: Maybe String
              , utmLoc         :: Loc
              , utmDur         :: Double
              , utmAmt         :: Double  -- delta change in seconds
              }
  |
  UtmRShift   { utmStaffN      :: Maybe String
              , utmLoc         :: Loc
              , utmDur         :: Double
              , utmAmt         :: Double  -- delta change in seconds
              }
  deriving(Show)    


data UtmRampShape = UrsFlat | UrsTowardEnd | UrsFromBeg
  deriving(Show)
               

data RampEndWhereOne = RewoBegin | RewoEnd


-- Time Adjusts move arrival times backward or forward in time.
-- This data represents an absolute change amount relative to 
-- base time map. (Rather than change in relative time map.)
-- 
-- <loc>  location of the mark
-- <String> staff name
-- <side> make the change in slice durations to left or right
--        sides
-- <dur>  duration of slices (in quarters) to make changes to 
-- <amt of absolute change from base time map in seconds>
data TimeAdjustAbs = TimeAdjustAbs Loc String WarpSide Double Double
-- Same thing as TimeAdjustRel but gives change in
-- slice group duration in relative time map.
data TimeAdjustRel = TimeAdjustRel Loc String WarpSide Double Double



-- Type of a function that processess a generic shape. The composition
-- auxilliary module (for a composition notes-01.sib, that is notes-01.hs)
-- will define a list of these.
type GsFunc = Map Int TimeSig -> GsCombined -> [Utm]


data BracketMark = 
    BmLeft String (Maybe String) [Double] 
  | BmCenter
  | BmRight String


brMarkName :: BracketMark -> String
brMarkName (BmLeft  n _ _) = n
brMarkName (BmRight n    ) = n

isPause (UnitPause _ _)             = True
isPause _                           = False

isAdjust (UnitAdjust _ _ _ _ _)     = True
isAdjust _                          = False

isWarpLocal (UnitWarp m _ _)        = isJust m
isWarpLocal _                       = False

isWarpGlob (UnitWarp m _ _)         = isNothing m
isWarpGlob _                        = False

isAbsWarp (UnitAbsWarp _ _ _)       = True
isAbsWarp _                         = False

isTwoModify (Unit2Modify _ _ _ _ _ _) = True
isTwoModify _                         = False


data Modif = ModifKs
  { modTiming :: Either Double Double  -- Left d means timing relative to
                 -- on time, Right d means timing relative to off time
  , modKey    :: Int
  }
           | ModifCtrl
  { modTiming :: Either Double Double
  , modCtrl   :: Int
  , modValue  :: Int
  }
           | ModifCtrlSet
  { modTiming    :: Either Double Double
  , modCtrlsVals :: [(Int,Int)]
  }
  deriving(Show)


data DestData = DestData
  { ddChanNum  :: (Int,Int)
  , ddPitch    :: Int
  , ddVel      :: Int
  , ddMods     :: [Modif]
  }

data MidiConfig = MidiConfig [MidiConfigOneDest]
  deriving(Show)
data MidiConfigOneDest = MidiConfigOneDest (Int,Int) [(Int,Int)]
  deriving(Show)
data MidiConfigFile = MidiConfigFile (Map String MidiConfig)
  deriving(Show)



----------------------------------------------------------------------

data TrillCard = TrillOdd
               | TrillEven

data LowLevelTrill =
    NoTrill 
  | SplitTrill
  { lltIsTremolo :: Bool
  , lltCount     :: Int      -- counts from zero
  , lltOfN       :: Int      -- number of notes in trill/tremolo including
                             -- both odd and even
  }
  | NonSplitTrill Int
  

-- <idx, 0-origin> <of total n> <tBeg> <tEnd>
data TrillData = TrillData Int Int Double Double


data TrRaw = TrRaw
  { trStaffName :: String
  , trTime      :: Double
  , trDest      :: (Int,Int)
  , trStatus    :: Int
  , trData1     :: Int
  , trData2     :: Int
  }


data SNote = SNote
  { snDescr     :: String
  , snHistory   :: [SNote]
  , snStaffName :: String
  , snLoc       :: Loc
  , snEnd2      :: Loc  -- for single notes, true end. for trill/trem, chord
                        -- end.
  , snVn        :: Int
  , snChord     :: Chord
  , snNote      :: Note
  , snOnOff     :: [(String,(Double,Double))]
  , snLoud      :: Double
  , snDest      :: (Int,Int)
  , snPitch     :: Int
  , snNomPitch  :: Int
  , snVel       :: Int
  , snMods      :: [Modif]
  , snAlterEnd  :: Double
  , snSepSame   :: Double
  , snTrill     :: LowLevelTrill
  , snPitWarpOffset :: Double
  }
  
data ControlCurve = ControlCurve 
  { _controlCurveDest    :: (Int,Int)
  , _controlCurveCtrlNum :: Int
  , _controlCurvePoints  :: [(Double,Int)]
  }

makeFields ''ControlCurve

data MetaPrepared = MetaPrepared
  { _metaPreparedAllSNotes :: Map String [SNote]
  , _metaPreparedRange     :: (Loc,Loc)
  }

makeFields ''MetaPrepared

type Tr = ExceptT String (State TrState)

data MetaInstr = forall s. MetaInstr
  { iName     :: String
  , iStaffNs  :: [String]
  , iData     :: s
  , iSplitTrillFlag :: Bool
  , iRun      :: MetaInstr -> s -> MetaPrepared -> Tr ()
  , iShapeFn  :: GsFunc
  }

data TrState = TrState
  -- constant
  { _trStateScore  :: Score
  , _trStateMetas  :: Map String MetaInstr

  -- random generator             
  , _trStateStGen    :: StdGen

  -- computed
  , _trStateTimeMaps      :: Map String AbsTimeMap
  , _trStateRelTimeMaps   :: Map String RelTimeMap
  , _trStateLoudnessMaps  :: Map String (Map Int Curve)
  , _trStateControlCurves :: [ControlCurve]
                     
  -- output
  , _trStateNotesOut :: [[SNote]]
  , _trStateRawsOut  :: [[TrRaw]]
  , _trStateInitRaws :: [[TrRaw]]
  , _trStateTimeMods :: Map String [UnitTimeMod]
  , _trStateDebugOut :: [String]
  }

makeFields ''TrState



data RunData = RunData [MetaInstr]


instance MyRandomClass TrState where
  getGen   = view stGen
  putGen g = modify $ (set stGen g)


