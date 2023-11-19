{-# LANGUAGE TupleSections, TypeSynonymInstances, FlexibleInstances,
    TemplateHaskell, FunctionalDependencies, MultiParamTypeClasses #-}
-- general Hollywood orchestra instrument. Two kinds of data: main
-- instrument and articulations
module Instruments.Hoopus where

import Data.Map(Map)
import qualified Data.Map as M
import Data.Set(Set)
import qualified Data.Set as S
import Data.Maybe
import Translation
import Translation.TimeMap
import Translation.AlterTimes
import Instruments
import Control.Monad
import Control.Monad.State
import Control.Lens
import Translation.InstrUtils
import Translation.AlterTimes
import qualified Translation.InstrUtils as InstrUtils
import Score.ScoreData
import Common
import Common.CommonUtil
import Util.Exception
import Common
import Common.CommonUtil
import Util.Exception
import Util.Showable
import Util.Map
import Translation.Curves
import Instruments


runHoopus :: MetaInstr -> Hoopus -> MetaPrepared -> Tr ()
runHoopus instr hoopus metaPre = do
  sc <- gets $ view score
  let stream = view streamm hoopus
      mergedMarks = InstrUtils.computeMergedMarks sc (iStaffNs instr)
      doStaff staffN = do
        let sns = hoopusLk staffN $ view allSNotes metaPre
        -- note: Oct. 2023: alterTOff must go here after
        -- computing all the SNotes rather than on each SNote
        mapM (hoopusDoNote hoopus mergedMarks) sns >>= alterTOff (view sepSame hoopus) >>= includeNotes 
  doStaff (view staffName hoopus)
  let (locB,locE) = view range metaPre
  -- send Mod controls
  forM_ (view modLoudnessDests hoopus) (\(ch,(modMin,modMax)) -> do
    let vc = VelCurve [(0.99,fromIntegral modMin),(8.01,fromIntegral modMax)]
    modRaws <- hoopusContinuousCtrl "hoopus mod" True False vc (stream,ch)
               (view staffName hoopus) locB locE
    includeRaws modRaws)
  -- send Expression (Ctrl 11) controls
  forM_ (view exprLoudnessDests hoopus) (\(ch,(exprMin,exprMax)) -> do
    let vc = VelCurve [(0.99,fromIntegral exprMin),(8.01,fromIntegral exprMax)]
    exprRaws <- hoopusContinuousCtrl "hoopus expr" False True vc (stream,ch)
               (view staffName hoopus) locB locE
    includeRaws exprRaws)


mkNoteDescr :: SNote -> Tr HoopusNoteDescr
mkNoteDescr snote = do
  slur <- isUnderSlur snote
  let (isTrill,isTrem) = case cNotes $ snChord snote of
        NSingles _              -> (0,False)
        NTrill (TtnTrill i) _ _ -> (i,False)
        NTrill TtnTremolo _ _   -> (0,True )
      modifiers = cModifiers $ snChord snote
  markedArtic <- hoopusMarkedArtic snote
  return $ HoopusNoteDescr markedArtic modifiers slur isTrill isTrem


hoopusDoNote :: Hoopus -> Map Loc [MarkD] -> SNote -> Tr SNote
hoopusDoNote hoopus mergedMarks snote = do
  -- ch :: HoopusChan
  noteD <- mkNoteDescr snote
  let ch = view determineChan hoopus noteD snote
      thisUpdateDest s = return s {snDest = (view streamm hoopus,view midiChan ch)}
  loudCurve <- gets $ hoopusLk (snStaffName snote) . view loudnessMaps
  updateLoud (view accentAmt ch) snote
    >>= hoopusSetNoteVel ch noteD
    >>= hoopusStaccato ch
    >>= hoopusEndAlter ch
    >>= updateArp [view staffName hoopus] mergedMarks
    >>= thisUpdateDest 
    >>= hoopusCtrlValues ch noteD


hoopusSetNoteVel :: HoopusChan -> HoopusNoteDescr -> SNote -> Tr SNote
hoopusSetNoteVel ch noteD s = case view velConfig ch of
  HVCLoudness curve -> return s { snVel = lookupVel "in hoopusSetNoteVel" 
                                          (snLoud s) curve}
  HVCFunc f         -> return s { snVel = f noteD s }


hoopusStaccato :: HoopusChan -> SNote -> Tr SNote
hoopusStaccato ch s 
  | view shortenForStaccato ch = updateStac s
  | otherwise                  = return s


hoopusEndAlter :: HoopusChan -> SNote -> Tr SNote
hoopusEndAlter ch = updateEndAlter False


hoopusCtrlValues :: HoopusChan -> HoopusNoteDescr -> SNote -> Tr SNote
hoopusCtrlValues ch noteD s = case view modConfig ch of
  HMCSelector f -> return s { snMods = newModif : snMods s }
    where 
      newModif = ModifCtrl (Left (-0.05)) 1 value
      value = f noteD s
  _             -> return s


hoopusMarkedArtic :: SNote -> Tr String
hoopusMarkedArtic sn = do
  sc <- gets $ view score :: Tr Score
  let marks  = hoopusLk (snStaffName sn) $ scMarksByStaff sc
      maybeArtic :: MarkD -> Maybe String
      maybeArtic (Artic s) = Just s
      maybeArtic _         = Nothing
      -- [(b,[a])] fl xs = do {(b,as) <- xs; a <- as; pure (b,a)}
      expList :: [(b,[a])] -> [(b,a)]
      expList xs = [(b,a) | (b,as) <- xs, a <- as]
      m2 :: [(Loc,MarkD)]  
      m2 = expList $ M.toDescList $ fst $ splitInclude (snLoc sn) marks
      maybeRight :: (Loc,MarkD) -> Maybe (Loc,String)
      maybeRight (a,b) = (a,) <$> maybeArtic b
      final :: Maybe (Loc,String)
      final = listToMaybe $ mapMaybe maybeRight m2 
  return $ case final of {Just (_,s) -> s; Nothing -> "h_ord"}


hoopusContinuousCtrl :: String -> Bool -> Bool -> VelCurve -> (Int,Int) ->
                       String -> Loc -> Loc -> Tr [TrRaw] 
hoopusContinuousCtrl msg includeMod includeExpr modCurve dest staffN locB locE = do
  lCurve <- (hoopusLkInt 1 . hoopusLk staffN) `liftM` gets (view loudnessMaps)
  tB <-  (lookupTime locB . hoopusLk staffN) `liftM` gets (view timeMaps)
  tE <-  (lookupTime locE . hoopusLk staffN) `liftM` gets (view timeMaps)
  let
      -- o: function to generate TrRaw's, given input argument of time
      o :: Double -> [TrRaw]
      o t = case curveLookup t lCurve of
        Just l  ->
          let
              v = lookupVel "in anySimpleMod, " l modCurve
              c1 = [ TrRaw staffN (t-0.002) dest 0xb0 1 v | includeMod ]
              c2 = [ TrRaw staffN (t-0.002) dest 0xb0 11 v | includeExpr]
          in
              c1 ++ c2
        Nothing -> []
  return $ concatMap o [tB,tB+0.05..tE] 


hoopusLk :: String -> Map String a -> a
hoopusLk k m = case M.lookup k m of {Just x -> x}

hoopusLkInt :: Int -> Map Int a -> a
hoopusLkInt k m = case M.lookup k m of {Just x -> x}
