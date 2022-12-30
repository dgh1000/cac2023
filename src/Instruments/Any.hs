
{-# LANGUAGE TupleSections, TypeSynonymInstances, FlexibleInstances,
    TemplateHaskell, FunctionalDependencies, MultiParamTypeClasses #-}

-- NOTES 2021
-- what can Any do?

-- data Any = Any
--  { _anyNotesFn :: Map Loc [MarkD] -> SNote -> Tr SNote
--  , _anyModFn   :: String -> Loc -> Loc -> Tr [TrRaw]
--  }



module Instruments.Any where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Translation.InstrUtils as InstrUtils
import Data.Set(Set)
import Debug.Trace
import Text.Printf
import Data.Map.Strict(Map)
import Data.List(sortBy)
import Data.Maybe
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Lens
import Data.Function
import Translation.InstrUtils
import Translation.TimeMap
import Translation.AlterTimes
import Translation
import Score.ScoreData
import Common
import Common.CommonUtil
import Util.Exception
import Util.Showable
import Util.Map
import Translation.Curves
import Instruments

-- any: useful for instruments which combine multiple articulations of same
-- instrument, clearly same instrument.
--
-- our different articulations could be considered different instruments.
--

-- can this handle garritan in which accents are hardness of accent and mod is
-- volume on most instruments?

-- NOTES 2021


data VelocitySource = VsCurve (String -> VelCurve)
                    | VsArticulation


data AnySimple = AnySimple
  { _anySimpleAccentAmt    :: Double
  , _anySimpleStacFn       :: SNote -> Tr SNote
  , _anySimpleEndAlterFn   :: SNote -> Tr SNote
  , _anySimpleArpFn        :: SNote -> Tr SNote
  , _anySimpleModFn        :: String -> Loc -> Loc -> Tr [TrRaw] -- staffN, b/e
  , _anySimpleDestFn       :: SNote -> String -> (Int,Int) -- artic
  , _anySimpleVelSource    :: VelocitySource
  , _anySimpleKsFn         :: SNote -> String -> Maybe Int -- artic input, maybe ks out
  , _anySimpleVolFn        :: String -> Either Int VelCurve
  , _anySimpleExprFn       :: String -> Either Int VelCurve
  , _anySimpleAliases      :: [(String,String)]
  , _anySimpleStaffN       :: String
  }


makeFields ''AnySimple


makeAny staffNs shapeFn accentAmt velCurve splitTrillFlag notesFn modFn =
     MetaInstr "synth" staffNs any splitTrillFlag runAny shapeFn
   where
     any = Any notesFn modFn


makeAnySimple :: String -> GsFunc -> AnySimple -> MetaInstr
makeAnySimple staffN shapeFn anySimp =
  MetaInstr staffN [staffN] (Any (anySimpleNotes anySimp) 
                                 (view modFn anySimp))
            False runAny shapeFn


-- using a synth meta instr means:
--
--   "patch": a combination of a midi channel and a set of midi controls to be
--   sent to that channel prior to playing notes
--
--   brackets will be used to surround notes to be played and specify a
--   conversion function
--
--   ---> notes not surrounded by brackets will not be played <---
--
--   ---> more than one bracket can surround a group of notes <---
--
-- 

runAny :: MetaInstr -> Any -> MetaPrepared -> Tr ()
runAny instr (Any nFn modFn) metaPre = do
  -- do staff: what's the common pattern here
  sc <- gets $ view score
  let mergedMarks = InstrUtils.computeMergedMarks sc (iStaffNs instr)
      doStaff staffN = do
        let sns = anyLk staffN $ view allSNotes metaPre
        mapM (nFn mergedMarks) sns >>= alterTOff >>= includeNotes 
        let (locB,locE) = view range metaPre
        modFn staffN locB locE >>= includeRaws
  mapM_ doStaff (iStaffNs instr) 


anyLk k m = case M.lookup k m of {Just x -> x}


-- anySimpleNotes
--
--   job of adding arpeggiation: probably will put it here
--
--   look at Piano for model. It calls updateArp, which needs a list of
--   staves (okay that's easy), marks, 
--
-- 
anySimpleNotes :: AnySimple -> Map Loc [MarkD] -> SNote -> Tr SNote 
anySimpleNotes anySimp mergedMarks sn = do
  artic <- anySimpleGetArtic anySimp sn
  
  let
      {-
      velCurve = view velCurveFn anySimp $ artic
      thisUpdateVel s = return s { snVel = lookupVel "in anySimpleNotes, "
                                       (snLoud s) velCurve }
      -}

 
      thisUpdateDest s = return s {snDest = (view destFn anySimp) s artic}
      thisUpdateCtrlMod :: SNote -> Tr SNote
      thisUpdateCtrlMod s = return $
        let x = case ((view ksFn anySimp) s artic) of
                  Nothing -> []
                  Just k  -> [ModifKs (Left (-0.05)) k]
            -- let x = maybeToList $
            --         fmap (ModifKs (Left $ -0.05)) $ ((view ksFn anySimp)  s artic)
            vm = ModifCtrl (Left $ -0.05) 7 (getVolume s)
            -- vm = ModifCtrl (Left $ -0.05) 7 (view volFn anySimp $ artic)
        in s { snMods = x ++ [vm] }
      getVolume s = case (view volFn anySimp) artic of
          Left i -> i
          Right c -> lookupVel "anySimpleNotes(volume)," (snLoud s) c
  loudCurve <- anyLk (snStaffName sn) `liftM` gets (view loudnessMaps)
  updateLoud (view accentAmt anySimp) sn
    >>= setNoteVelocity anySimp artic
    >>= view stacFn anySimp
    >>= view endAlterFn anySimp
    >>= updateArp [view staffN anySimp] mergedMarks
    >>= thisUpdateDest
    >>= thisUpdateCtrlMod


-- setNoteVelocity
--
--   Set the velocity of a note
--
--        either by using the loudness
--
--        or by using the articulation.
--
--
setNoteVelocity :: AnySimple -> String -> SNote -> Tr SNote
setNoteVelocity anySimple artic s = case view velSource anySimple of
  VsArticulation -> 
    return s { snVel = getArticulationLevel . cModifiers $ snChord s }
  VsCurve f      -> 
    return s { snVel = lookupVel "in setNoteVelocity, "
                       (snLoud s) (f artic) }


-- getArticulationLevel
--
--   Choose a velocity as used in Garritan to specify attack level.
--
--   Base this velocity on articulations that are present on the note's
--   chord.
getArticulationLevel :: Set ChordModifier -> Int
getArticulationLevel ms =
  if Accent `elem` ms
    then 80
    else
      if DownBow `elem` ms
        then 95
        else
          if Tenuto `elem` ms
            then 50
            else
              if UpBow `elem` ms
                then 35
                else 64

-- any_updateVel :: AnySimple -> SNote -> SNote


-- anySimpleModExample
--
--   A generic implementation of mod control that can be used by most
--   instruments.
--
--   This generates
--
--     a list of mod values (as TrRaw data) from 'locB' to
--     'locE'
--
--   The mod value is derived from the loudness curve
--
--     and put through the VelCurve 'modCurve' to get a MIDI value.
--
anySimpleModExample :: String -> Bool -> VelCurve -> (Int,Int) ->
                       String -> Loc -> Loc -> Tr [TrRaw] 
anySimpleModExample msg includeExpr modCurve dest staffN locB locE = do
  
  lCurve <- (anyLk 1 . anyLk staffN) `liftM` gets (view loudnessMaps)
  
  tB <-  (lookupTime locB . anyLk staffN) `liftM` gets (view timeMaps)
  tE <-  (lookupTime locE . anyLk staffN) `liftM` gets (view timeMaps)
  
  let
      -- o: function to generate TrRaw's, given input argument of time
      o :: Double -> [TrRaw]
      o t = case curveLookup t lCurve of
        Just l  ->
          let
              v = lookupVel "in anySimpleMod, " l modCurve
              
              c1 = [ TrRaw staffN (t-0.002) dest 0xb0 1 v ]
              
              c2 = if includeExpr
                     then [ TrRaw staffN (t-0.002) dest 0xb0 11 v ]
                     else []
          in
              c1 ++ c2
              
        Nothing -> []
        
  return $ concatMap o [tB,tB+0.05..tE] 


{-
anySimpleDest :: AnySimple -> SNote -> Tr (Int,Int)
anySimpleDest simp sn = do
  a <- anySimpleGetArtic sn
  return $ (view destFn simp) a 
-}

anySimpleGetArtic :: AnySimple -> SNote -> Tr String
anySimpleGetArtic anySimp sn = do
  let modSet = let x = cModifiers $ snChord sn :: Set ChordModifier
               in x
  markedArtic <- anySimpleMarkedArtic sn
  let isStac = Staccato `elem` modSet
      isMarc = Marcato  `elem` modSet
      (isTrill,isTrem) = case cNotes $ snChord sn of
        NSingles _              -> (0,False)
        NTrill (TtnTrill i) _ _ -> (i,False)
        NTrill TtnTremolo _ _   -> (0,True )
  let rawA = case (isTrill,isTrem,isStac,isMarc) of
             (1,_,_,_)    -> "tr1"
             (2,_,_,_)    -> "tr2"
             (_,True,_,_) -> "trem"
             (_,_,_,True) -> "marc"
             (_,_,True,_) -> "stac"
             _        -> markedArtic
  return $ case lookup rawA (view aliases anySimp) of
    Just a   -> a
    Nothing -> rawA


anySimpleMarkedArtic :: SNote -> Tr String
anySimpleMarkedArtic sn = do
  sc <- gets $ view score :: Tr Score
  let marks  = anyLk (snStaffName sn) $ scMarksByStaff sc
      maybeArtic :: MarkD -> Maybe String
      maybeArtic (Artic s) = Just s
      maybeArtic _         = Nothing
      -- [(b,[a])] fl xs = do {(b,as) <- xs; a <- as; pure (b,a)}
      expList :: [(b,[a])] -> [(b,a)]
      expList xs = [(b,a) | (b,as) <- xs, a <- as]
      m2 :: [(Loc,MarkD)]  
      m2 = expList $ M.toDescList $ fst $ splitInclude (snLoc sn) marks
      maybeRight :: (Loc,MarkD) -> Maybe (Loc,String)
      maybeRight (a,b) = fmap (a,) $ maybeArtic b
      final :: Maybe (Loc,String)
      final = listToMaybe $ mapMaybe maybeRight m2 
  return $ case final of {Just (_,s) -> s; Nothing -> "ord"}

