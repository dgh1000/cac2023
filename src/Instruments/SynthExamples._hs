{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances, TypeSynonymInstances #-}

module Instruments.SynthExamples where

import Text.Printf
import Debug.Trace
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Translation
import Score.ScoreData
import Instruments
import qualified Instruments.Synth as Synth
import qualified Translation.TimeMap as TimeMap
import qualified Translation.InstrUtils as InstrUtils
import Translation.InstrUtils
import Control.Lens
import Control.Monad.State
import Common

--   


-- basic10-kb-01
--
--   filter ADSR: 14, 15, 16, 17
--
--   ampl ADSR: 18, 19, 20, 21
--
--   filter cutoff: 22
--
--




-- 


----------------------------------------------------------------------
-- examples of SynthChan






-- chanMap1 = M.fromList [ ("moog", chanFalcon01) ]


----------------------------------------------------------------------
--
-- SimpleBracketFunc
--
--   We will define this function:
--
--     simpleBracketFunc :: SimpleBracketFunc -> BracketFunc
--
--   It handles bracket function functionality, but in a simple way. All notes
--   are converted to MIDI events as follows:
--
--     bracket default values
--
--     dest is the field in SimpleBracketFunc data, 'dest'
--
--     velocity is determined by loudness marked in the score
--
--     snMods is left empty
--
--     alterEnd is determined in same way as Piano
--
--     snSepSame is determined by field 'sepSame' in SimpleBracketFunc
--
--  Additional note: we assume 'iSplitTrillFlag' in the MetaInstr is set to
--  False, so all trill SNotes arrive at the bracket function having already
--  been split.



{-
test_lookupSynthChan :: SimpleBracketFunc -> MetaInstr -> Synth ->
                        BracketParams -> SynthChan
test_lookupSynthChan sbf meta synth params = c
  where
    c = seLookup (view chanName sbf) (view chans synth)

test_lookupControls :: SimpleBracketFunc -> MetaInstr -> Synth ->
                        BracketParams -> NamedCtrlNums
test_lookupControls sbf meta synth params = view ctrls c
  where
    c = test_lookupSynthChan sbf meta synth params

-}

-- simpleBracketFunc.
--
--  BRACKET FUNCTIONS
--
--    functions that follow an algorithm to convert notes to MIDI: that means
--
--      determing channel: this is read from MARK ON STAFF
--
--      determining velocity
--
--      determining volume related control curve
--
--      determining beginning control settings - this is configured
--
--      determine separation or legato: this is READ FROM SLURS, ext MARKS and
--      trunc MARKS
--
--
--  THIS IS A SIMPLE bracket function that uses SIMPLEBRACKETFUNC data
--
--     We will produce output Tr ([TrRaw],[SNote])
--
--     Generates [TrRaw] by calling simpleBracketFunc_doRaws
--
--
--     Generates [SNote] by
--
--       For each input SNote in (view notes params)
--
--         Set its SNote.snLoud field to loudness at this point on staff
--
--         Set its SNote.snVel field to corresponding velocity using the vel
--         curve (view velCurve sbf)
--
--         Change length of staccato notes
--
--         Extend end of legato notes (or truncate non-legato)
--
--         Set dest to (view chanDest synthChan)
--
-- some notes to a particular channel while having some basic control
-- settings. we could also have the loudness curve sent to a control channel.
--
simpleBracketFunc :: SimpleBracketFunc -> BracketFunc
simpleBracketFunc sbf meta synth params = do
  let updateVel :: SNote -> Tr SNote
      updateVel s =
        return $ s { snVel = InstrUtils.lookupVel "e29582" (snLoud s) c }
        where
          c = view velCurve sbf
      updateDest s = return $ s { snDest = view dest sbf }
      doOneSNote :: SNote -> Tr SNote
      doOneSNote s = updateLoud (view synth accent) s >>=
                     updateVel >>=
                     updateStac >>= updateEndAlter >>=
                     updateDest
  sNotes <- mapM doOneSNote (view notes params)
  rawsOut <- simpleBracketFunc_doRaws sbf params
  return (rawsOut,sNotes)  


-- simpleBracketFunc_doRaws
--
--   generate raws
--
--       Control settings that occur at time of Loc 'view begLoc params'
--       specific to this type of bracket.
--
--         we change that loc to a time via calling (lookupTime loc atm) where
--         atm comes from: first get Map at (view timeMaps `liftM` get)=m then
--         call (seLookup (view staffN params) m) to get atm
--
--       we find the values to set as (view defValues sbf). This is a map of
--       NamedCtrlValues.
--
--       we find the control numbers in the 'synthChan' as (view ctrls
--       synthChan)
--
--       we also generate a series of controls corresponding to loudness if
--       (view loudCtrl sbf) is not None
--
-- 

{-

REMOVED THIS BECAUSE controls will now be part of bracket functions

simpleBracketFunc_doCtrlSettings ::
  (Int, Int) -> SynthChan ->  BracketParams -> Tr [TrRaw]
simpleBracketFunc_doCtrlSettings dest chan params =
  do atms <- view timeMaps `liftM` get
     sc   <- view score `liftM` get
     let atm = seLookup (view staffN params) atms
         -- t0 = TimeMap.lookupTime (view begLoc params) atm
         -- t1 = TimeMap.lookupTime (view begLoc params) atm
         allMarks =
           lookupControlSettingsMarks
             (view staffN params)
             (view begLoc params)
             (view endLoc params)
             (scMarksByStaff sc)
         go :: (Loc, String) -> [TrRaw]
         go (loc, name) = map mk cs
           where
             cs :: [CtrlNumPlusValue]
             cs = seLookup name $ view ctrlSets chan
             mk (ctrlNum, value) =
               TrRaw
                 staffN
                 (view loc atm)
                 (view dest chan)
                 0xb0
                 ctrlNum
                 value
     return $ concatMap go allMarks

-}


-- how do we test this? dump raws. organize them somehow? by dest, time,
-- control number and value
{-

     NOTES: FOR FIXING UP DORAWS

data SimpleBracketFunc = SimpleBracketFunc
  { _simpleBracketFuncChanName      :: String
  , _simpleBracketFuncCtrlSettings  :: [(Int, Int)]
  , _simpleBracketFuncSep           :: Double
  , _simpleBracketFuncLegato        :: Double
  , _simpleBracketFuncVelCurve      :: VelCurve
  , _simpleBracketFuncLoudCtrl      :: Maybe (Int,VelCurve)
  }
-}

--

simpleBracketFunc_doNamedCtrls :: Synth -> BracketParams -> Tr [TrRaw]
simpleBracketFunc_doNamedCtrls synth params = do
  let b = view begLoc params
      e = view endLoc params
  error "do named ctrls"
  


simpleBracketFunc_doRaws ::
  SimpleBracketFunc -> BracketParams -> Tr [TrRaw]
simpleBracketFunc_doRaws sbf params = do
  atms <- view timeMaps `liftM` get
  let atm = seLookup (view staffN params) atms
      t0 :: Double
      t0 = TimeMap.lookupTime (view begLoc params) atm
      t1 = TimeMap.lookupTime (view endLoc params) atm
      staffName = view staffN params
      -- BRACKET PARAMS include a Maybe <control name>.
      --
      theDest = view dest sbf
      doRaw :: (Int, Int) -> TrRaw
      doRaw
        (ctrlNum, val) =
          TrRaw
            staffName (t0 - 0.05) theDest 0xB0 ctrlNum val
      defaultRaws :: [TrRaw]
      defaultRaws = map doRaw (view ctrlSettings sbf)

      oneLoudRaw :: Int -> VelCurve -> Double -> Tr TrRaw
      oneLoudRaw cNum c t = do
        l <- InstrUtils.basicLookupLoud t 1 staffName
        let v = InstrUtils.lookupVel "e27382" l c
        return $ TrRaw staffName (t-0.05) theDest 0xB0 cNum v
        
  loudRaws <- case view loudCtrl sbf of
        Nothing          -> return []
        Just (cNum,velC) -> mapM (oneLoudRaw cNum velC) [t0,t0+0.05..t1]
  return $ defaultRaws ++ loudRaws


-- why shouldn't bracket functions be associated with control sets? well it's
-- simpler to 

-- note: the bracket data will provide the staff name and beg and end locs
--
simpleBracketMod :: VelCurve -> (Int,Int) -> Int ->
                    String -> Loc -> Loc -> Tr [TrRaw] 
simpleBracketMod modCurve dest ctrlNum staffN locB locE = error "foo"

seLookup :: Ord k => k -> Map k a -> a
seLookup k m = case M.lookup k m of {Just x -> x}






