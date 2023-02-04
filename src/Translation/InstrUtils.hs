
module Translation.InstrUtils where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Set(Set)
import Debug.Trace
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Except
import Control.Lens
import Data.Map.Strict(Map)
import Data.Maybe
import Text.Printf
import Common
import Common.CommonUtil
import Score.ScoreData
import Translation
import Translation.ShowTranslation
import Translation.TimeMap
import Translation.Curves
import Translation.Trill
import Util.Map
import Util.Exception
import Util.Showable
import Util.Math


-- allStaffSNote
--
--   Convert all notes on a staff to **initial** form of SNote
--
--   Initial form does not have certain fields set to valid values
--
--   Some examples of what are not set:
--
--     destination, velocity, mods
--
--   --> timing is set to a nominal value and might be altered later
--
-- 
--  data SNote = SNote
--    { snDescr     :: String                 **SET**
--    , snHistory   :: [SNote]                **SET**
--    , snStaffName :: String                 **SET**
--    , snLoc       :: Loc                    **SET**
--    , snEnd2      :: Loc  -- for single notes, true end. for trill/trem, chord
--                          -- end.           **SET**
--    , snVn        :: Int                    **SET**
--    , snChord     :: Chord                  **SET**
--    , snNote      :: Note                   **SET**
--    , snOnOff     :: [(String,(Double,Double))] **SET** to nominal values
--                                                  using timemap of staff
--    , snLoud      :: Double                 **SET**
--    , snDest      :: (Int,Int)             ** NOT ** SET **
--    , snPitch     :: Int                    **SET**
--    , snNomPitch  :: Int                    **SET**
--    , snVel       :: Int                 **NOT SET NOT SET**
--    , snMods      :: [Modif]             **NOT SET NOT SET**
--    , snAlterEnd  :: Double
--    , snSepSame   :: Double
--    , snTrill     :: LowLevelTrill
--    }
--  

allStaffSNote :: String -> Bool -> Int -> Int -> Tr [SNote]
allStaffSNote staffN splitTrillFlag msrBeg msrEnd = do
  -- 9/2/22: we select 'chords' value from the appropriate staff named staffN
  chords <- (stChords . iuLookup "aaa" staffN . scStaves) `liftM`
            gets (view score)
  -- 9/2/22: do one Loc: this function can be mapped over M.toAscList chords
  let doLoc :: (Loc,Map Int Chord) -> Tr [SNote]
      doLoc (loc@(Loc msr _),m)
        | msrBeg <= msr && msr <= msrEnd =
            concat `liftM` (mapM doVoice $ M.toList m)
        | otherwise = return []
        where
          doVoice :: (Int,Chord) -> Tr [SNote]
          doVoice (vn,c) = case cNotes c of
            NSingles notes -> doNSingles staffN loc vn c notes
            NTrill ttn ns1 ns2 -> doNTrill splitTrillFlag staffN loc
                                  vn c ttn ns1 ns2
  concat `liftM` (mapM doLoc $ M.toAscList chords)


doNSingles :: String -> Loc -> Int -> Chord -> Map Int Note -> Tr [SNote]
doNSingles staffN loc vn ch notes =
  mapM (initSNote staffN loc vn ch) $ M.elems notes


initSNote :: String -> Loc -> Int -> Chord -> Note -> Tr SNote
initSNote staffN loc vn ch n@(Note pit _ trueEnd _) = do
  atm <- gets (iuLookup "h" staffN . view timeMaps)
  return SNote { snDescr      = "init"
               , snHistory    = []
               , snStaffName  = staffN
               , snLoc        = loc
               , snEnd2       = trueEnd
               , snVn         = vn
               , snChord      = ch
               , snNote       = n
               , snOnOff      = [ ( "nominal"
                                  , ( lookupTime loc atm
                                    , lookupTime trueEnd atm)
                                  )
                                ]
               , snLoud       = -1
               , snDest       = (-1,-1)
               , snPitch      = midiPitch pit
               , snNomPitch   = midiPitch pit
               , snVel        = -1
               , snMods       = []
               , snAlterEnd   = 0
               , snSepSame    = 0.01
               , snTrill      = NoTrill
               , snPitWarpOffset = 0
               }
  


doNTrill :: Bool -> String -> Loc -> Int -> Chord -> TrillTremNote ->
            Map Int Note -> Map Int Note -> Tr [SNote]
doNTrill splitTrillFlag staffN loc vn ch ttn ns1 ns2
  | splitTrillFlag = doSplitTrill staffN loc vn ch ttn ns1 ns2
  | otherwise = liftM (:[]) $ doNonSplitTrill staffN loc vn ch ttn ns1 ns2


doNonSplitTrill :: String -> Loc -> Int -> Chord -> TrillTremNote ->
                       Map Int Note -> Map Int Note -> Tr SNote
doNonSplitTrill staffN loc vn ch ttn ns1 ns2 = do
  let n = case M.elems ns1 of
        [x] -> x
  templateNote <- initSNote staffN loc vn ch n
  let outTrill = case ttn of
        TtnTrill i -> NonSplitTrill i
  return $ templateNote { snDescr   = "nonSplitTrill"
                        , snHistory = templateNote : snHistory templateNote
                        , snTrill   = outTrill
                        }
  

doSplitTrill :: String -> Loc -> Int -> Chord -> TrillTremNote ->
                    Map Int Note -> Map Int Note -> Tr [SNote]
doSplitTrill staffN loc vn ch ttn ns1 ns2 = do
  templateNote <- initSNote staffN loc vn ch (iuMinElem ns1)
  atm <- iuLookup "tt" staffN `liftM` gets (view timeMaps)
  marks <- (iuLookup "xx" staffN . scMarksByStaff) `liftM` gets (view score)
  let lk mpred msg = lookupStaffMarkLE mpred msg loc marks
      mShape = case ttn of
        TtnTremolo -> lk isTremShape  "'tremolo shape'"
        TtnTrill _ -> lk isTrillShape "'trill shape'" 
      shape = case mShape of
          Nothing -> throwMine $ printf ("no trill or tremolo shape prior "++
                     "to %s on staff %s") (showLoc2 loc) staffN
          Just s -> s
  let firstNoteEnd = case M.minView ns1 of
        Just (n,_) -> nTrueEnd n
  let tBeg = lookupTime loc atm
      tEnd = lookupTime firstNoteEnd atm
      (data1,data2) = trillTimes shape tBeg tEnd
      g :: Bool -> TrillData -> Note -> SNote
      g isTrem (TrillData i n t1 t2) note@(Note pit _ trueEnd _) =
        templateNote
          { snDescr    = "splitTrill"
          , snHistory  = templateNote : snHistory templateNote
          , snNote     = note
          , snOnOff    = ("splitTrill",(t1,t2)) : snOnOff templateNote
          , snPitch    = midiPitch pit
          , snNomPitch = midiPitch pit
          , snTrill    = SplitTrill isTrem i n
          }
      flag = case ttn of {TtnTremolo -> True; TtnTrill _ -> False}
  return $ [g flag d note | d <- data1, note <- M.elems ns1] ++
           [g flag d note | d <- data2, note <- M.elems ns2]
           
---------------------------------------------------------------------
-- acciaccatura and appogiatura
doAcciAppo :: String -> Tr ()
doAcciAppo staffN = do
  graces <- (stGrace . iuLookup "aaa" staffN . scStaves) `liftM`
            gets (view score)
  return ()

data SNoteModInstr  = SNMITruncate Loc Double
                    | SNMIDecapitate Loc Double

{-
oneLocAcciAppo :: String -> Int -> AbsTimeMap -> (Loc,(AcciSeq,AppoSeq)) -> 
                  ([SNote],[SNoteModInstr])
oneLocAcciAppo staffN vn atm (loc,(acciSeq,appoSeq)) = error "foo"
  where
    t = lookupTime loc atm
    appoSep = 0.1
    -- forward sequence of numbers
    -- midi pitch, position 
    --   position counts 0, 1, 2, for appo and -1, -2, -3 for acci
    doOne :: Int -> Int -> SNote 
    doOne pit pos = s
      where
        (t1,t2) = ( t + fromIntegral pos * appoSep
                  , t + fromIntegral pos * appoSep + 2*appoSep)
        s = SNote { snDescr      = "init"
                  , snHistory    = []
                  , snStaffName  = staffN
                  , snLoc        = loc
                  , snEnd2       = loc
                  , snVn         = vn
                  , snChord      = Chord loc S.empty (NSingles M.empty)
                  , snNote       = (Note (Pitch pit 0 0 0) False loc NormalHead)
                  , snOnOff      = [ ("nominal", (t1,t2)) ]
                  , snLoud       = -1
                  , snDest       = (-1,-1)
                  , snPitch      = pit
                  , snNomPitch   = pit
                  , snVel        = -1
                  , snMods       = []
                  , snAlterEnd   = 0
                  , snSepSame    = 0.01
                  , snTrill      = NoTrill
                  , snPitWarpOffset = 0
                  }
-}

----------------------------------------------------

iuMinElem :: Map k a -> a
iuMinElem m = case M.minView m of
  Just (x,_) -> x

----------------------------------------------------------------------
--                    lookup loudness


computeCtrls :: String -> (Int,Int) -> Tr [TrRaw]
computeCtrls staffN dest = do
  atm <- iuLookup2 "a" staffN `liftM` gets (view timeMaps)
  marks <- (iuLookup "zz" staffN . scMarksByStaff) `liftM` gets (view score)
  let f :: Loc -> [MarkD] -> [TrRaw]
      f loc marksAtLoc =
          map (\(ctrl,val) -> TrRaw staffN t dest 0xB0 ctrl val) ctrls
        where
          ctrls = mapMaybe maybeCtrl marksAtLoc
          t = lookupTime loc atm
  return $ concat $ M.mapWithKey f marks
  


maybeCtrl :: MarkD -> Maybe (Int,Int)
maybeCtrl (MidiCtrl ctrl _ val) = Just (ctrl,val)
maybeCtrl _                = Nothing



----------------------------------------------------------------------

  

computeAccent2 amt mods loud
  | Accent `elem` mods = loud+amt
  | Tenuto `elem` mods = loud-amt
  | DownBow `elem` mods = loud-2*amt
  | UpBow `elem` mods = loud+2*amt
  | otherwise          = loud


clipLoud l | l < globMinLoud = "warning: min loud" `trace` globMinLoud
           | l > globMaxLoud = "warning: max loud" `trace` globMaxLoud
           | otherwise = l


----------------------------------------------------------------------   


iuLookupLE :: Ord k => k -> Map k a -> a
iuLookupLE k m = case M.lookupLE k m of {Just (_,v) -> v}
              

----------------------------------------------------------------------

includeNotes :: [SNote] -> Tr ()
includeNotes evts = case evts of
  [] -> "no notes on a particular staff, just a warning" `trace` return ()
  xs -> modify (over notesOut (xs:))


includeInitRaws :: [TrRaw] -> Tr ()
includeInitRaws evts = modify (over initRaws (evts:))
  -- \s -> s {tsInitRaws = evts:tsInitRaws s})


includeRaws :: [TrRaw] -> Tr ()
includeRaws evts = modify (over rawsOut (evts:))
   -- \s -> s {tsRaws = evts:tsRaws s})


----------------------------------------------------------------------

----------------------------------------------------------------------
--                   time- and Loc- related


arpCount :: [String] -> Loc -> Int -> Tr (Int,Int)
arpCount staffNames loc pitch = do
  pitches <- concat `liftM` mapM (arpPitches loc) staffNames
  let sortedUp   = L.sort pitches
      sortedDown = reverse sortedUp
      eIdx xs = case L.elemIndex pitch xs of
        {Just idx -> idx}
  return (eIdx sortedUp,eIdx sortedDown)


arpPitches :: Loc -> String -> Tr [Int]
arpPitches loc staffName = do
  sc <- gets $ view score
  let chordPitches (Chord _ mods (NSingles ps) _) = M.keys ps
      chordPitches (Chord _ mods (NTrill _ ps1 ps2) _) = M.keys ps1 ++ M.keys ps2
      cp c | Arpeggiate `elem` cModifiers c = chordPitches c
           | otherwise = []
      chordLookup :: Loc -> Map Loc (Map Int Chord) -> Map Int Chord
      chordLookup loc chords = case M.lookup loc chords of
        Just cs -> cs
        Nothing -> M.empty
  return . concatMap cp . M.elems . chordLookup loc . stChords .
           iuLookup "h" staffName $ scStaves sc



----------------------------------------------------------------------
--                general-purose translation helpers


iuLookup :: (Show k, Ord k) => String -> k -> Map k a -> a
iuLookup s k m = case M.lookup k m of
  Just x -> x
  Nothing -> throwMine $ "at " ++ s ++ ", key:" ++ (show k)



----------------------------------------------------------------------
isShort :: Chord -> Bool
isShort (Chord _ mods _ _) = Staccato `elem` mods


----------------------------------------------------------------------
--                  map utils

-- staff mark EQ? what is this? EQ to existing time
lookupStaffMarkEQ :: (MarkD -> Maybe a) -> String -> Loc -> Map Loc [MarkD] ->
                     Maybe a
lookupStaffMarkEQ g msg loc marks = out
  where
    out = case M.lookup loc marks of
      Nothing -> Nothing
      Just ms -> case mapMaybe g ms of
        []  -> Nothing
        [x] -> Just x
        _   -> throwMine $ printf "multiple %s data at %s" msg (showLoc2 loc)


lookupStaffMarkLE :: (MarkD -> Maybe a) -> String ->
                     Loc -> Map Loc [MarkD] -> Maybe a
lookupStaffMarkLE g msg loc marks =
  lookupPriorStaffMarkData' g msg loc marks


-- lookupControlSettingsMarks
--
lookupControlSettingsMarks ::
  String -> Loc -> Loc -> Map String (Map Loc [MarkD]) -> [(Loc, String)]
lookupControlSettingsMarks
  staffN begLoc endLoc m = mapMaybe ms2 $ M.toList m3
  where
    m2 = iuLookup "3874" staffN m
    m3 :: Map Loc [MarkD]
    m3 = M.filterWithKey pred m3
    pred l _ = begLoc <= l && l < endLoc
    maybeCtrlSetting :: MarkD -> Maybe String
    maybeCtrlSetting (CtrlSetting s) = Just s
    maybeCtrlSetting _               = Nothing
    ms :: [MarkD] -> Maybe String
    ms marks = listToMaybe $ mapMaybe maybeCtrlSetting marks
    ms2 :: (Loc,[MarkD]) -> Maybe (Loc,String)
    ms2 (loc,marks) = case ms marks of
      Nothing -> Nothing
      Just s  -> Just (loc, s)


iiuLookup :: Ord k => String -> k -> Map k a -> a
iiuLookup msg k m = case M.lookup k m of
  Just x -> x
  Nothing -> throwMine $ "iiulookup: " ++ msg

lookupPriorStaffMarkData' :: (MarkD -> Maybe a) -> String -> Loc ->
                            Map Loc [MarkD] -> Maybe a
lookupPriorStaffMarkData' g msg loc m = out
  where
    (m1,_) = splitInclude loc m
    toData ms = case mapMaybe g ms of
      []   -> Nothing
      [d]  -> Just d
      _    -> throwMine $ printf ("multiple %s data at loc %s")
              msg (showLoc2 loc)
    m3 :: [(Loc,[MarkD])]
    m3 = M.toDescList m1
    out = case mapMaybe (toData . snd) m3 of
      []  -> Nothing
      x:_ -> Just x
      
      

----------------------------------------------------------------------

lookupVel :: String -> Double -> VelCurve -> Int
lookupVel errMsg l (VelCurve segs) = case L.find matches segPairs of
  Nothing -> throwMine $ printf ("%s: loudness of %5.2f is not " ++
    "covered by any segments in VelCurve") errMsg l
  Just ((x1,y1),(x2,y2)) -> round $ scale x1 l x2 y1 y2
  where
    matches ((t1,_),(t2,_)) = t1 <= l && l <= t2    
    segPairs = zip segs (drop 1 segs)


----------------------------------------------------------------------

findStaffBegEndLocs :: String -> Int -> Int -> Tr (Loc,Loc)
findStaffBegEndLocs staffN begMsr endMsr = do
  chords <- (stChords . iuLookup2 "b" staffN . scStaves) `liftM`
            gets (view score)
  let cs = snd . splitInclude (Loc endMsr 20) . fst .
           splitInclude (Loc begMsr 1) $ chords
      begLoc = case M.minViewWithKey cs of
        Just ((k,v),_) -> k

  
  error "foo"

-- chordsMaxTrueEnd :: Map Loc (Map Int Chord) 

----------------------------------------------------------------------

arpOffsets size = scanl (+) 0 (map s [0..size-1])
  where
    s i = scale 0 (fromIntegral i) (fromIntegral $ size-1) 0.8 1.2 

-- arpOffsets = [0.0,0.6,1.4,2.4,3.6,4.8,6.4,8.1]

aoLk :: Int -> Int -> Double
aoLk size n | n >= 0 = (arpOffsets size) !! n


-- returns the amount to alter t1 for arpeggio
genericArp :: [String] -> Loc -> Int -> Double -> Tr Double
genericArp staffNs loc pit arpDelta = do
  let lkArpOffsets up dn n = abs arpDelta * aoLk (up+dn+1) n
  (up,dn) <- arpCount staffNs loc pit
  let leadAmt | arpDelta >= 0 =  lkArpOffsets up dn dn
              | arpDelta  < 0 =  lkArpOffsets up dn up
  return $ -leadAmt


isTrillShape :: MarkD -> Maybe TrillShape
isTrillShape (TrillShapeMark s) = Just s
isTrillShape _                   = Nothing


isTremShape :: MarkD -> Maybe TrillShape
isTremShape (TremShapeMark s) = Just s
isTremShape _                  = Nothing


isArtic :: MarkD -> Maybe String
isArtic (Artic s) = Just s
isArtic _          = Nothing
  

isStac :: MarkD -> Maybe Double
isStac (StacDur d) = Just d
isStac _            = Nothing

isCtrlSetting :: MarkD -> Maybe String
isCtrlSetting (CtrlSetting s) = Just s
isCtrlSetting _               = Nothing

isExt (Extend d) = Just d
isExt _           = Nothing


isTrunc (Trunc d) = Just d
isTrunc _          = Nothing


isLTrunc (LTrunc d) = Just d
isLTrunc _           = Nothing


isArpDelta (ArpDelta d) = Just d
isArpDelta _             = Nothing


isBracketL (BracketL s _ _) = Just s
isBracketL _                = Nothing


isBracketR (BracketR s) = Just s
isBracketR _             = Nothing


anyTrueEnd :: Notes -> Loc
anyTrueEnd notes = case notes of
  NSingles m -> case M.minView m of
    Just (note,_) -> nTrueEnd note
  NTrill _ m1 _ -> case M.minView m1 of
    Just (note,_) -> nTrueEnd note


mkMod :: String -> Curve -> VelCurve -> Double -> Double -> Double ->
         Double -> [(Int,Int)] -> [TrRaw]
mkMod staffN loudCurve velCurve t1 t2 delta lead dests =
    [mk x y | x <- ts, y <- dests]
  where
    ts = [t1,t1+delta..t2]
    mk t dest = case curveLookup t loudCurve of
      Nothing -> throwMine $ printf ("in constructing mod curve, missing "++
                 "dynamics mark before first data point")
      Just loud -> TrRaw staffN (t-lead) dest 0xb0 0x01 vel
        where
          vel = lookupVel "in mkMod" loud velCurve

----------------------------------------------------------------------
--             utitiled for building up SNote data

-- takes voice number from note. basic lookup takes as param. both iuLookup2
-- to get loudness map for that staff and voice number.
updateLoud :: Double -> SNote -> Tr SNote
updateLoud accentAmt s = do
  louds <- gets (view loudnessMaps)
  let staffN = snStaffName s
      -- through x = ("abcdefg: " ++ showLoudnessMaps louds) `trace` x -- MUSESCORE
      loc = snLoc s
  lcs <- (iuLookup3 "c" (snVn s) . iuLookup2 "d" staffN) 
         `liftM` gets (view loudnessMaps)
  let loud1 = case curveLookup (getTOn s) lcs of
        Nothing -> throwMine $ printf ("missing dynamic mark on staff " ++
                   "'%s' before %s") staffN (showLoc2 loc)
        Just x -> x
      loud = computeAccent2 accentAmt (cModifiers $ snChord s) loud1
  return s { snHistory = s : snHistory s
           , snDescr   = "updateLoud"
           , snLoud = loud }

showLoudnessMaps :: Map String (Map Int Curve) -> String
showLoudnessMaps m1 = show $ M.map f1 m1
  where
    f1 :: Map Int Curve -> Map Int String
    f1 = M.map (const "Some curve") 

getLoudValue :: Double -> SNote -> Tr Double
getLoudValue accentAmt s = do
  let staffN = snStaffName s
      loc = snLoc s
  lcs <- (iuLookup2 "e" (snVn s) . iuLookup2 "f" staffN)
         `liftM` gets (view loudnessMaps)
  let loud1 = case curveLookup (getTOn s) lcs of
        Nothing -> throwMine $ printf ("missing dynamic mark on staff " ++
                   "'%s' before %s") staffN (showLoc2 loc)
        Just x -> x
  return $ computeAccent2 accentAmt (cModifiers $ snChord s) loud1
  


-- 
basicLookupLoud :: Double -> Int -> String -> Tr Double
basicLookupLoud t vn staffN = do
  curve <- (iuLookup2 "g" vn . iuLookup2 "h" staffN) `liftM` gets (view loudnessMaps)
  return $ case curveLookup t curve of
             Nothing -> throwMine $ "error in basicLookupLoud: perhaps no " ++
                               "dynamic mark " ++ show t
             Just x  -> x
        

updateStac :: SNote -> Tr SNote
updateStac s = do
  marks <- (iuLookup2 "i" (snStaffName s) . scMarksByStaff) `liftM`
           gets (view score)
  return $ if Staccato `elem` (cModifiers $ snChord s)
             then let stacDur = case lookupStaffMarkLE isStac "'stac'"
                                     (snLoc s) marks of
                                  Just x -> x
                                  Nothing -> throwMine $
                                             printf ("missing stac dur " ++
                                             "mark on staff '%s' before " ++
                                             "%s") (snStaffName s)
                                             (showLoc2 $ snLoc s) 
                  in s { snDescr = "updateStac"
                       , snHistory = s : snHistory s
                       , snOnOff = ("stac",(getTOn s,getTOn s+stacDur))
                                   : snOnOff s
                       }
             else s




-- we need a function to determine if a note is under a slur
--
isUnderSlur :: SNote -> Tr Bool
isUnderSlur s = do
  let staffN = snStaffName s
      beg  = snLoc s
      end2 = snEnd2 s
  score <- gets $ view score
  let slurs = let s = stSlurs . iuLookup2 "z" staffN $ scStaves score
              in s
      isMarkedSlur = case M.lookupLT end2 slurs of
        Nothing -> False
        Just (_,endSlur) | end2 <= endSlur   -> True
                         | otherwise         -> False
  return isMarkedSlur
  

-- updateEndAlter
--
--   This figures out if a note is UNDER A SLUR. It also figures out WHAT THE
--   APPLICABLE 'ext' or 'trn' mark is.
--
-- 
updateEndAlter :: Bool -> SNote -> Tr SNote
updateEndAlter alwaysSlur s = do
  let staffN = snStaffName s
      beg  = snLoc s
      end2 = snEnd2 s
  score <- gets $ view score
  let slurs = let s = stSlurs . iuLookup2 "w" staffN $ scStaves score
              in s
      staffMarks = iuLookup2 "x" staffN $ scMarksByStaff score
      isMarkedSlur' = case M.lookupLT end2 slurs of
        Nothing -> False
        Just (_,endSlur) | end2 <= endSlur   -> True
                         | otherwise         -> False
      isMarkedSlur = isMarkedSlur' || alwaysSlur
      lk pred locX markName =
        case lookupStaffMarkLE pred markName locX staffMarks of
          Just x -> x
          Nothing -> throwMine $ printf
                     "needs a %s mark on staff %s" markName staffN
      stac = lk isStac beg "'stac'"
      ext  = lk isExt  beg "'ext'"
      trunc = case lookupStaffMarkEQ isLTrunc "'ltrn'" end2 staffMarks of
        Nothing -> lk isTrunc beg "'trn'"
        Just x  -> x
      isMarkedStac = Staccato `elem` (cModifiers $ snChord s)
      isTrillFlag = case snTrill s of
        NoTrill -> False
        _       -> True
                     
      a = case (isTrillFlag,isMarkedStac,isMarkedSlur) of
        (True,_,_)          -> ext
        (False,True,_)      -> 0
        (False,False,True)  -> ext
        (False,False,False) -> -trunc
  return $ s { snDescr = "updateEndAlter"
             , snHistory = s : snHistory s
             , snAlterEnd = a
             }

updateArp :: [String] -> Map Loc [MarkD] -> SNote -> Tr SNote
updateArp a b s@SNote{snChord = c}
  | Arpeggiate `elem` (cModifiers c) = updateArp' a b s
  | otherwise = return s

updateArp' :: [String] -> Map Loc [MarkD] -> SNote -> Tr SNote
updateArp' staffNs mergedMarks s = do
  let loc = snLoc s
      arpDelta = case lookupStaffMarkLE isArpDelta "'arp'" loc mergedMarks of
                   Just x -> x
                   Nothing -> throwMine $ printf ("missing arp delta mark "++
                     "before %s") (showLoc2 loc)
  alter <- genericArp staffNs loc (snPitch s) arpDelta
  let t1 = getTOn s
      t2 = getTOff s
      d  = t2-t1
      t1a = t1+alter
      t2a = min t2 (t1a+1.5*d)
  return s { snDescr = "updateArp"
           , snHistory = s : snHistory s
           , snOnOff = ("updateArp",(t1a,t2a)) : snOnOff s
           }


iuLookup2 msg k m = case M.lookup k m of
  Just x -> x
  Nothing -> error msg

-- lcs <- (iuLookup3 "c" (snVn s) . iuLookup2 "d" staffN) 

iuLookup3 :: String -> Int -> Map Int a -> a
iuLookup3 msg k m = case M.lookup k m of
  Just x -> x
  Nothing -> throwMine $ printf "Voice not found in loudness maps is %d" k

getTOn :: SNote -> Double
getTOn s = case snOnOff s of {x:_ -> fst $ snd x}


getTOff :: SNote -> Double
getTOff s = case snOnOff s of {x:_ -> snd $ snd x}


computeMergedMarks :: Score -> [String] -> Map Loc [MarkD]
computeMergedMarks score staffNs =
  M.unionsWith (++) . map snd .
  filter (\(n,_) -> n `elem` staffNs) . M.toList $
  scMarksByStaff score
  
                     
showTrRaws :: [TrRaw] -> ShowItem
showTrRaws raws = Component "Raws" False (map showI raws)
