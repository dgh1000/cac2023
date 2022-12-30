module Instruments.Piano where

import qualified Data.Map.Strict as M
import Data.Map.Strict(Map)
import Text.Printf
import Data.Set(Set)
import Data.Maybe
import Control.Monad
import Control.Monad.State
import Control.Lens
import Instruments
import Translation
import Translation.InstrUtils
import Translation.TimeMap
import Translation.Trill
import Translation.Curves
import Translation.AlterTimes
import Common
import Common.CommonUtil
import Score.ScoreData
import Util.Exception
import Util.Map

defCtrlSettings :: Map String [Int]
defCtrlSettings = M.fromList [("1", [21]),("2",[22]), ("3",[23]), ("4",[24]), ("5",[25]),
  ("6",[26]), ("7",[27])]

makePiano :: String -> Double -> Map String (Int,Int) -> Double ->
             VelCurve -> VelCurve -> Int -> GsFunc -> Bool -> MetaInstr
makePiano name pedOffset dests accentAmt v1 v2 volume gf allLegato =
     MetaInstr name (M.keys dests)
               (Piano2 dests accentAmt v1 v2 pedOffset volume allLegato defCtrlSettings) True pianoRun gf


-- 9/2/22
--   metaPre: contains SNotes and beg/end Loc
--     NOTE: the 
pianoRun :: MetaInstr -> Piano2 -> MetaPrepared -> Tr ()
pianoRun instr pno2 metaPre = do
  let staffNs = iStaffNs instr
  doInit (iStaffNs instr) pno2
  -- set up merged Mark3
  score <- gets $ view score
  let mergedMarks = M.unionsWith (++) . map snd .
                    filter (\(n,_) -> n `elem` staffNs) . M.toList $
                    scMarksByStaff score
  mapM (doPnoStaff instr pno2 mergedMarks metaPre) staffNs
    >>= (alterTOff . concat)
    >>= includeNotes 
  computePedal pno2 metaPre staffNs >>= includeRaws
  
-- 9/2/22 notes on the Tr state monad
-- TrState input: Score, all MetaInstr, TimeMap, Loudness maps, Control curves, 
-- TrState output: [[SNote]], [[TrRaw]], UnitTimeMod

-- 9/2/22 notes on MetaPrepared
--   has "all" SNote - not sure what is meant by "all" - all within entire range of 
--     composition? are times present?
--   has range (Loc,Loc) : presumably the range over which we take final SNote to play

doPnoStaff :: MetaInstr -> Piano2 -> Map Loc [MarkD] ->
              MetaPrepared -> String -> Tr [SNote]
doPnoStaff instr pno2 mergedMarks metaPre staffN = do 
  let staffNs = iStaffNs instr
      doSNote :: SNote -> Tr SNote
      -- 9/2/22 notes in preparation for adding acci/appo 
      --   this takes SNotes which are partially set and updates
      --     - loudness : I think this is loudness 1 to 8 and accounts for accents
      --     - velocity : computed from loudness using velocity curves
      --     - stac
      --     - endAlter
      --     - arpeggio
      --     - control settings: not sure. probably used with synthesizers
      --     - destination
      --   if we convert acci/appo to SNotes, which of these do we want to use?
      --     - loudness
      --     - velocity
      --     - destination
      --   
      doSNote s = updateLoud (pno2AccentAmt pno2) s >>=
                  pnoUpdateVel pno2 >>=
                  updateStac >>= updateEndAlter (pno2AllLegato pno2) >>=
                  updateArp staffNs mergedMarks >>=
                  pnoUpdateCtrlSetting pno2 >>=
                  pnoUpdateDest pno2
  -- so here we are looking up just the staff
  mapM doSNote . pLookup staffN $ view allSNotes metaPre
  -- here we want to generate SNotes for acci/appo and modify any SNotes that
  --    need to be modified in timing to provide room for appo


  -- 9/2/22 old from above
  -- -- allStaffSNote staffN True msrBeg msrEnd >>= mapM doSNote

{-
doStaffCtrls :: Piano2 -> String -> Tr ()
doStaffCtrls pno2 staffN = do
  let dest = pLookup staffN $ pno2Dests pno2
      vol = TrRaw staffN 0 dest 0xb0 7 (pno2Vol pno2)
  includeInitRaws [vol]
-}    

doInit :: [String] -> Piano2 -> Tr ()
doInit names pno2 = do
  let mkInitRaw stName =
        TrRaw stName 0 (pLookup stName $ pno2Dests pno2) 0xB0 7 (pno2Vol pno2)
  -- look up staff names
  includeInitRaws . map mkInitRaw $ names
  


pLookup k m = case M.lookup k m of {Just x -> x}


pnoUpdateVel :: Piano2 -> SNote -> Tr SNote
pnoUpdateVel pno2 s = do
  let l | snLoud s >= 0 = snLoud s
      velCurve = case snTrill s of
        NoTrill          -> pno2VCurveSingle pno2
        SplitTrill _ _ _ -> pno2VCurveTrill  pno2
  return s { snDescr   = "pianoUpdateVel"
           , snHistory = s : snHistory s
           , snVel     = lookupVel "in piano2, " l velCurve
           }


pnoUpdateDest :: Piano2 -> SNote -> Tr SNote
pnoUpdateDest pno s = 
  return s { snDescr = "pnoUpdateDest"
           , snHistory = s : snHistory s
           , snDest  = pLookup (snStaffName s) $ pno2Dests pno
           }

-- making a piano note involves what?
--
-- for each Chord
--
--   okay we know that trills are different than tremolo, different than
--   single
--
-- looks like loudness is best done on TrNote because its done on the on/off
-- times

pnoUpdateCtrlSetting :: Piano2 -> SNote -> Tr SNote
pnoUpdateCtrlSetting pno2 s = do
  marks <- (iuLookup2 "xx" (snStaffName s) . scMarksByStaff) `liftM` gets
              (view score)
  let mName = lookupStaffMarkLE isCtrlSetting "'ctrlSetting'" 
            (snLoc s) marks
      ret = case mName of
        Nothing -> s
        Just name  -> modifyCtrlSettings s name
  return ret
  
        
modifyCtrlSettings :: SNote -> String -> SNote
modifyCtrlSettings 
  s@SNote { snMods = modsIn }
  name = s { snMods = modsIn ++ map toModifCtrl ctrlsValues }
  where
    ctrlsValues = case M.lookup name defCtrlSettings of
      Just vs -> vs
      Nothing -> error $ "undefined ctrl setting: " ++ name
    toModifCtrl :: Int -> Modif
    toModifCtrl key = ModifKs (Left (-0.1)) key


                          

computePedal :: Piano2 -> MetaPrepared -> [String] -> Tr [TrRaw]
computePedal pno2 metaPre staffNs = do
  score <- gets $ view score
  let doStaff :: String -> Tr [TrRaw]
      doStaff staffN = do
        atm <- pLookup staffN `liftM` gets (view timeMaps)
        let evts = trim (view range metaPre) . stPedalEvts . pLookup staffN $
                   scStaves score
            dest = pLookup staffN $ pno2Dests pno2
            toPedalEvt (loc,pe) = out
              where
                t = lookupTime loc atm
                t1 = t-0.012
                -- t2 = t+pno2PedalChangeDelta pno2
                t2 = t+0.1
                out = case pe of
                  PedalStart  -> [ TrRaw staffN t  dest 0xB0 64 127 ]
                  PedalStop   -> [ TrRaw staffN t  dest 0xB0 64 0   ]
                  PedalChange -> [ TrRaw staffN t1 dest 0xB0 64 0
                                 , TrRaw staffN t2 dest 0xB0 64 127 ]
        return . concatMap toPedalEvt $ M.toAscList evts
  concat `liftM` mapM doStaff staffNs

pnoUpdateMidiCtrls :: Piano2 -> SNote -> Tr SNote
pnoUpdateMidiCtrls pno snote = do
  error "foo"

-- input is map of Loc to pedal events. We need to trim this to given start
-- and end measures.
--
-- boils down to (1) possibly add pedal start at first measure, (2) possibly
-- add pedal stop to just after last note.
--
-- could do this to raw events, add pedal stop after last note off.
--
-- one question? is there a pedal start hanging over?
--

-- trim. then did we chop off a pedal off that should be there? did we add a
-- pedal start that should be there?


trim :: (Loc,Loc) -> Map Loc PedalEvt -> Map Loc PedalEvt
trim (locBeg,locEnd) mIn = f1 . f2 $ trimmed
  where
    trimmed = M.filterWithKey
              (\k _ -> locBeg <= k && k < locEnd) mIn
    f1 m | didChopEnd (locBeg,locEnd) mIn =
             M.insert locEnd PedalStop m
         | otherwise = m
    f2 m | didChopBeg  (locBeg,locEnd) mIn =
             M.insert locBeg PedalStart m
         | otherwise = m


dumpPedEvts :: Map Loc PedalEvt -> String
dumpPedEvts m = unlines $ map g $ M.toAscList m
  where
    g (k,v) = printf "%s %s" (showLoc2 k) (show v)


didChopEnd :: (Loc,Loc) -> Map Loc PedalEvt -> Bool
didChopEnd (locB,locE) mIn = 
  case M.toDescList $ fst $ M.split locE mIn of
    (_,PedalStart ):_ -> True
    (_,PedalChange):_ -> True
    _                 -> False


didChopBeg :: (Loc,Loc) -> Map Loc PedalEvt -> Bool
didChopBeg (locB,_) mIn
  | isJust $ M.lookup locB mIn = False
  | otherwise                            = flag
  where
    flag = case M.toDescList $ fst $ M.split locB mIn of
      (_,PedalStart ):_ -> True
      (_,PedalChange):_ -> True
      _                 -> False
