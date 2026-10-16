
{-# LANGUAGE TupleSections, FlexibleInstances, TemplateHaskell,
    FunctionalDependencies #-}


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

-- Oct. 2026: 
--
-- Goal is to update Any and AnySimple to 
-- (1) generalize the modFn to include control marks. 
--     control marks CC number and data will be based on a SendCtrl mark.
--     we need to base the control marks' destinations on MidiChan marks
--     or the default channel. the ctrl function taking the staff name
--     will be enough to find MidiChan marks
-- (2) modify the notes function to update their destinations based
--     on MidiChan marks. 
--
-- Where do these things take place in the current system? 
--
-- (1) modFn as currently stands: 
--   - type Any has a modFn (just changed to ctrlFn) with the signature
--       ctrlFn :: String -> Loc -> Loc -> Tr [TrRaw]
--     - is this enough? well it gives us staff name so we can look up marks.
--       limits of Loc. we can still make mod marks if we want.
--   - type AnySimple has a mod function too. 
--   - does AnySimple have any other functions for sending control events? There is
--     modifFn which computes the modifiers to a given SNote which can sometimes
--     take the form of a CC message. But these are note-attached and not attached
--     to marks
--     - there is expr function which might influence what CC 11 events are sent.
--       those would be handled by the existing "mod functions"
--     - there is 'configs'. how are those used? a grep shows that everywhere 
--       in the code those are initialized to M.empty. It might have something
--       to do with bracket functions or shape functions, but has never been
--       implemented.
-- (2) what functions in Any or AnySimple influence the computation of notes?
--     in particular the dests. 
--   - stacFn, endAlterFn, arpFn :: SNote -> Tr SNote 
--       (I wonder how arpFn works without reference to Score? maybe it's passed
--        though parial application. Same with stacFn and endAlterFn? Why don't 
--        these need staff name? ah, staff name is in SNote)
--   - accentAmt :: Double
--   - destFn :: SNote -> String -> (Int,Int)
--       (I wonder how this is configured. Look this up eventually.)
--   - velSource :: VelocitySource. not sure what this does. It can take VsCurve
--       or VsArticulation. If VsArticulation is there another way to configure it?
--   - ksFn :: SNote -> String -> Maybe Int
--      the string is an articulation. I think this was how I was using QL Orchestra
--      in those compositions from 2023.
--   - modifFn :: SNote -> String -> [Modif] 
--      this seems to refer to choosing the articulation, I think. either through
--      keyswitch or CC value. Clearly note attached. But I don't know if
--      I intended this to be used for CC articulations only, as keyswitches would
--      have come through the ksFn.
--   - volFn, exprFn :: String -> Either Int VelCurve. String is articulation, so
--      I could have separate velocity curves for each articulation.
--
-- How I should implement these two changes: (CC marks and midi chan marks)
-- 
--   use the existing destFn but update it's type signature to take staff name
--   as well as artic. 

data VelocitySource = VsCurve (SNote -> String -> VelCurve)
                    | VsArticulation


data AnySimple = AnySimple
  { _anySimpleAccentAmt    :: Double
  , _anySimpleStacFn       :: SNote -> Tr SNote
  , _anySimpleEndAlterFn   :: SNote -> Tr SNote
  , _anySimpleArpFn        :: SNote -> Tr SNote
  , _anySimpleCtrlFn       :: String -> Loc -> Loc -> Tr [TrRaw] -- staffN, b/e
  , _anySimpleDestFn       :: SNote -> String -> (Int,Int) -- artic
  , _anySimpleVelSource    :: VelocitySource
  , _anySimpleKsFn         :: SNote -> String -> Maybe Int -- artic input, maybe ks out
  -- NOTE 2023: volume control events are attached to SNotes rather than being
  --   separate raws, so there cannot be a smooth curve of vol. 
  , _anySimpleModifFn      :: SNote -> String -> [Modif]
  , _anySimpleVolFn        :: String -> Either Int VelCurve
  -- NOTE 2023: The following, exprFn, is never used.
  , _anySimpleExprFn       :: String -> Either Int VelCurve
  , _anySimpleAliases      :: [(String,String)]
  , _anySimpleStaffN       :: String
  , _anySimpleConfigs      :: Map String [((String,Int),Int)]
  }


makeFields ''AnySimple


makeAny staffNs shapeFn accentAmt velCurve splitTrillFlag notesFn modFn =
     MetaInstr "synth" staffNs any splitTrillFlag runAny shapeFn
   where
     any = Any notesFn modFn


-- Change Sept. 13, 2023: added the argument splitTrillFlag so I 
--   can pass it along to Meta 
--
-- Oct. 2026. as I prepare to make a synth instrument
-- for driving the Moog. We want to send mod values from dynamics along
-- with midi velocity. I guess they have to correspond. If we want them
-- functioning separately we can a different Moog instrument.
-- Anywhere in this function we would choose the channel for SNotes? Anywhere
-- we send mod values? SendCtrl values? We use functions in AnySimple.
-- what's difference between Any and AnySimple? I don't understand 
-- how the fields of AnySimple are accessed beside anysimpleNotes and modFn.
-- Any has only a notes function and a mods function
--
makeAnySimple :: String -> GsFunc -> Bool -> AnySimple -> MetaInstr
makeAnySimple staffN shapeFn splitTrillFlag anySimp =
  MetaInstr staffN [staffN] (Any (anySimpleNotes anySimp) 
                                 (view ctrlFn anySimp))
            splitTrillFlag runAny shapeFn


-- change Oct. 2026. I'm getting rid of bracket functions. To specify synth
-- setting, my plan is to use MidiChan marks and SendCtrl marks. We'll just 
-- use a lot of them if needed. A good start.
--
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

-- Oct. 2026: run any takes only an Any which has a notes function
-- and a mods function. Is that enough? What about keyswitches?
-- Oh those are attached to notes. What about SendCtrl? Would that be covered
-- in ModFn? oh I have those attached to notes. I need to send them
-- at the loc they are attached to so I can send them on an empty staff.
-- An example would be Pedal in piano.
-- Okay it looks like mod function is called for entire composition playback
-- range. but it assumes to use the dest fixed to that staff, rather than
-- a dest indicated by previous channel mark.
runAny :: MetaInstr -> Any -> MetaPrepared -> Tr ()
runAny instr (Any nFn modFn) metaPre = do
  -- do staff: what's the common pattern here
  sc <- gets $ view score
  let mergedMarks = InstrUtils.computeMergedMarks sc (iStaffNs instr)
      doStaff staffN = do
        let sns = anyLk staffN $ view allSNotes metaPre
        mapM (nFn mergedMarks) sns >>= alterTOff True >>= includeNotes 
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
--   how does it fine # marking for keyswitch?
--
-- Oct. 2026: need to find associated midi channel for notes. Wait 
-- we already have a destFn. Can that be incorporated? This seems to
-- have been built-in to accomodate Hollywood orchestra. 
anySimpleNotes :: AnySimple -> Map Loc [MarkD] -> SNote -> Tr SNote 
anySimpleNotes anySimp mergedMarks sn = do
  -- is 'artic' what we are calling thet # function?
  -- calls anySimpleGetArtic
  artic <- anySimpleGetArtic anySimp sn
  
  let
      {-
      velCurve = view velCurveFn anySimp $ artic
      thisUpdateVel s = return s { snVel = lookupVel "in anySimpleNotes, "
                                       (snLoud s) velCurve }
      -}

 
      thisUpdateDest s = return s {snDest = view destFn anySimp s artic}
      thisUpdateCtrlMod :: SNote -> Tr SNote
      thisUpdateCtrlMod s = return $
        -- ksFn passing marked artic. I see now 
        let x = case view ksFn anySimp s artic of
                  Nothing -> []
                  Just k  -> [ModifKs (Left (-0.05)) k]
            y = view modifFn anySimp s artic
            -- let x = maybeToList $
            --         fmap (ModifKs (Left $ -0.05)) $ ((view ksFn anySimp)  s artic)
            vm = ModifCtrl (Left $ -0.05) 7 (getVolume s)
            -- vm = ModifCtrl (Left $ -0.05) 7 (view volFn anySimp $ artic)
        in s { snMods = x ++ y ++ [vm] }
      getVolume s = case view volFn anySimp artic of
          Left i  -> i
          Right c -> lookupVel "anySimpleNotes(volume)," (snLoud s) c
  loudCurve <- gets $ anyLk (snStaffName sn) . view loudnessMaps
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
                       (snLoud s) (f s artic) }


-- getArticulationLevel
--
--   Choose a velocity as used in Garritan to specify attack level.
--
--   Base this velocity on articulations that are present on the note's
--   chord.
getArticulationLevel :: Set ChordModifier -> Int
getArticulationLevel ms
  | Accent `elem` ms = 80
  | DownBow `elem` ms = 95
  | Tenuto `elem` ms = 50
  | UpBow `elem` ms = 35
  | otherwise = 64

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
              c2 = [ TrRaw staffN (t-0.002) dest 0xb0 11 v | includeExpr]
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
  {-
  let isStac = Staccato `elem` modSet
      isMarc = Marcato  `elem` modSet
  -}
  let(isTrill,isTrem) = case cNotes $ snChord sn of
        NSingles _              -> (0,False)
        NTrill (TtnTrill i) _ _ -> (i,False)
        NTrill TtnTremolo _ _   -> (0,True )
  let rawA = case (isTrill,isTrem) of 
        (1,_)     -> "tr1"
        (2,_)     -> "tr2"
        (_,True)  -> "trem"
        _         -> markedArtic
  {-
  let rawA = case (isTrill,isTrem,isStac,isMarc) of
             (1,_,_,_)    -> "tr1"
             (2,_,_,_)    -> "tr2"
             (_,True,_,_) -> "trem"
             (_,_,_,True) -> "marc"
             (_,_,True,_) -> "stac"
             _            -> markedArtic
  -}
  return $ fromMaybe rawA (lookup rawA (view aliases anySimp))


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
      maybeRight (a,b) = (a,) <$> maybeArtic b
      final :: Maybe (Loc,String)
      final = listToMaybe $ mapMaybe maybeRight m2 
  return $ case final of {Just (_,s) -> s; Nothing -> "ord"}

