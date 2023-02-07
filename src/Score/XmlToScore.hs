{-# LANGUAGE TupleSections, ScopedTypeVariables #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
module Score.XmlToScore where

import qualified Data.List as L
import qualified Data.Set as S
import qualified XmlDoc.XmlDocData as XD
import qualified Data.Map as M
import Control.Arrow
import Debug.Trace
import Data.Array
import Data.Maybe ( listToMaybe, catMaybes, isJust, mapMaybe )
import Data.Map(Map)
import Data.Set(Set)
import Text.Printf
import Score.ScoreData
import Score.ParseMarks(computeWordMarks)
import XmlDoc.XmlDocExport
import XmlDoc.Process(computeXmlStaves)
import Score.XmlToScore_grace (splitGrace)
import Score.XmlToScore_tnote
import Score.XmlToScore_ties
import Common
import Util.Exception
import Util.Map
import Common.CommonUtil
import XmlDoc.ParseXml (parseIsSib)
import Util.Showable
import XmlDoc.XmlDocData


xmlToScore :: XScore -> Score
xmlToScore xscore =
  Score 
   { scTimeSigs       = timeSigs
   , scMarks          = allMarks
   , scMarksByStaff   = marksByStaff
   , scMarkers        = M.map computeMarkers marksByStaff
   , scStaves         = staves
   , scUsedMsrs       = S.unions . map stUsedMsrs . 
                        M.elems $ staves
   }
  where
    
    result1 = computeXmlStaves xscore
    xmlStaves:: Map String (Map Loc [XMsrData])
    xmlStaves = snd result1
    xMsrInfo = fst result1
    timeSigs = addAnotherMsr $
               M.map (\(IXMsrInfo _ numer denom) -> TimeSig numer denom)
               xMsrInfo
    -- DATA:
    --     xmlStaves is Map String (Map Loc [XMsrData]). 
    -- RESULT:
    --      stavesWords is filtering out WordDirections
    stavesWords :: Map String (Map Loc [WordDirection])
    stavesWords = M.map (lMapMaybe maybeWord) xmlStaves
    
    -- we need to compute Map Loc (Map String [Mark]
    
    allMarks'    = computeWordMarks stavesWords
    staves       = M.mapWithKey 
      (computeStaff xMsrInfo timeSigs) $ xmlStaves
    allMarks     = foldInMetSymbolMarks staves allMarks'
    marksByStaff = computeMarksByStaff (M.keys staves) allMarks

    {-
    staves2 = M.intersectionWith insertBrackets
             (M.map matchBrackets marksByStaff) staves
    insertBrackets :: Map String [(Loc,Loc)] -> Staff -> Staff
    insertBrackets m s = s {stBrackets = m}
    -}



----------------------------------------------------------------------
----------------------------------------------------------------------
--                time sig fudging

addAnotherMsr :: TimeSigs -> TimeSigs
addAnotherMsr ts = case M.maxViewWithKey ts of
  Just ((maxMsr,timeSig),_) -> M.insert (maxMsr+1) timeSig ts

----------------------------------------------------------------------
----------------------------------------------------------------------
--                   mark computationsd

computeMarksByStaff :: [String] -> Map Loc (Map String [MarkD]) ->
                       Map String (Map Loc [MarkD])
computeMarksByStaff names mIn = foldl f (flipMap mIn) names
  where
    f m name | M.member name m = m
             | otherwise       = M.insert name M.empty m



-- 
foldInMetSymbolMarks :: Map String Staff -> Map Loc (Map String [MarkD]) ->
                        Map Loc (Map String [MarkD])
foldInMetSymbolMarks staves marksIn =
  
  -- what we do is take each staff. each staff will have metronone and
  -- symbol marks in a map with Loc as the key and the values are a list of
  -- marks.. But in the score, we represent the marks with Loc as the key
  -- and a submap of score name. We for each staff, we need to convert
  -- 
  --    Map Loc [MarkD] to Map String (Map Loc [MarkD])
  --
  -- 
  
  
    foldl merge marksIn . map g $ M.toAscList staves
  where
    g :: (String,Staff) -> Map Loc (Map String [MarkD])
    g (staffName,staff) = M.map (M.singleton staffName) $ stMetSymMarks staff
    merge :: Map Loc (Map String [MarkD]) -> Map Loc (Map String [MarkD]) ->
             Map Loc (Map String [MarkD])
    merge = M.unionWith (M.unionWith (++))


----------------------------------------------------------------------
----------------------------------------------------------------------
--                basic staff creation function


-- computeStaff
--
--   marks
--
--     computing them requires two phases
--
--       goal of phase 1: gather macro definitions from all staves. this is
--       done by translation to Pass1Word first, which will pull out macro
--       definitions and instances but not translate any other marks
--
--       goal of phase 2: change 
--
--
--  problem: some marks will be specific to a staff, while some will be paired
--  with marks which are on a different staff
-- 
computeStaff :: Map Int IXMsrInfo -> Map Int TimeSig -> String -> 
                Map Loc [XMsrData] -> Staff
computeStaff msrInfo timeSigs staffName xmlStaff1 =
    Staff { stName           = staffName
          , stDynamics       = computeDynamics staffNums xmlStaff
          , stHairpins       = computeHairpins staffNums xmlStaff
          , stPedalEvts      = computePedalEvts xmlStaff
          , stMetSymMarks    = metSym
          , stMaxTrueEnd     = computeMaxTrueEnd (error "foo") -- chords
          , stUsedMsrs       = S.fromList . map msrNum . M.keys $ prelimChords
          , stSlurs          = computeSlurs xmlStaff
          , stChords         = error "foo" 
          --  stGrace        :: Map Loc (Map Int GraceNoteSeq)
          , stGrace          = M.empty -- toGraceNoteSeq graces
          , stVoiceToStaff   = d2 
          , stStaffToVoice   = d1

          }
  where
    -- splitGrace :: Map Loc [XMsrData] -> 
    --           (Map Loc [XMsrData], Map Loc [XMsrData])
    -- toGraceNote :: XMsrData -> GraceNote
    -- toGraceNoteSeq :: Map Loc [GraceNote] -> Map Loc GraceNoteSeq
    -- toGraceNoteSeq = M.map toGraceNoteSeq2

    (xmlStaff,graces) = splitGrace xmlStaff1
    allXmlData :: [XMsrData]
    allXmlData = concat $ M.elems xmlStaff
    staffNums :: [Int]
    staffNums = S.toAscList . S.fromList . mapMaybe staffNumOf $ allXmlData
    (d1,d2) = computeStavesToVoicesAndVoicesToStaves xmlStaff
    metSym = metAndSymbolMarks xmlStaff
    -- this looks at what XMsrData? XMDNote will have grace notes
    prelimChords = computeChords msrInfo xmlStaff
    chordEndsMap = computeChordEndsMap prelimChords
    -- exploring 9/1/22: the two things that get us from a prelimChord to a Chord is
    --   processing which needs a chordEndsMap (not sure what a chord ends map is
    --   or why it is needed) and needing metronome marks and symbols
    -- so we don't include grace notes
    -- chords       = prelimChord2Chord metSym $
    --                processTies chordEndsMap prelimChords
    -- graceNotes = computeGrace msrInfo xmlStaff 


computeStavesToVoicesAndVoicesToStaves :: Map Loc [XMsrData] -> (Map Int [Int],Map Int Int)
computeStavesToVoicesAndVoicesToStaves xmsrs = (d4,d6)
  where
    -- XMsrData -> Maybe (staffNum,voiceNum) -- is Just only if object has both a staff number
    --                                          and a voice number
    through :: Show a => String -> a -> a
    through msg x = (msg ++ " " ++ show x) `trace` x
    g :: XMsrData -> Maybe (Int,Int)
    g x = case (staffNumOf x,voiceNumOf x) of
      (Just sn,Just vn) -> Just (sn,vn)
      _                 -> Nothing

    d1 :: [(Int,Int)] -- [(staff num,voice num)]
    d1 = mapMaybe g $ concat $ M.elems xmsrs
    d2 :: [(Int,Set Int)]
    d2 = map (\(a,b) -> (a,S.singleton b)) d1
    d3 :: Map Int (Set Int)
    d3 = M.fromListWith S.union d2
    d4 :: Map Int [Int] -- Map <staff num> <all voices on that staff>
    d4 = M.map S.toAscList d3
    d5 :: [(Int,Int)] -- [(voice num, staff num)]
    d5 = map (\(x,y) -> (y,x)) d1
    d6 :: Map Int Int -- voice num to staff num it appears on
    d6 = M.fromList d5


staffNumOf :: XMsrData -> Maybe Int
staffNumOf (XMDDirection _ _ _ x _) = x
staffNumOf (XMDNote n _) = xnStaff n
staffNumOf _ = Nothing

voiceNumOf :: XMsrData -> Maybe Int
voiceNumOf (XMDDirection _ _ x _ _) = x
voiceNumOf (XMDNote n _) = xnVoice n
voiceNumOf _ = Nothing

{-
numberGraceNotes :: [XMsrData] -> [XMsrData]
numberGraceNotes = snd . L.mapAccumL go 0
  where
    -- (s -> a -> (s, b)) -> s -> t a -> (s, t b)
    go :: Int -> XMsrData ->(Int,XMsrData)
    go x w@(XMDNote n@XNNote{}) = case xnIsGrace n of
      Just _  -> (x+1,XMDNote n {xnGraceOrder = x})
      Nothing -> (x,w)
    go x d = (x,d)
-}

computeMaxTrueEnd :: Map Loc (Map Int Chord) -> Loc
computeMaxTrueEnd chords =
  case concatMap (ends . cNotes) . concatMap M.elems . M.elems $ chords of
    xs@(x:_) -> maximum xs
  where
    ends (NSingles ns) = map nTrueEnd $ M.elems ns
    ends (NTrill _ ns1 ns2) = map nTrueEnd (M.elems ns1) ++
                              map nTrueEnd (M.elems ns2)


maybeWord :: XMsrData -> Maybe WordDirection
maybeWord (XMDDirection (XDWords s y) _ _ _ _) =
  case y of
    Just jy | jy > -7   -> Just $ WdAbove s
            | otherwise -> if s == "aps:12"
                             then "aha" `trace` Just $ WdBelow s
                             else Just $ WdBelow s
    Nothing -> Just $ WdAbove s
maybeWord _ = Nothing


----------------------------------------------------------------------
----------------------------------------------------------------------
--                 octave shift lines


{-

                    xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx

                        January 28, 2107 LEAVING OUT OCTAVE LINES



computeOctShift :: Map Loc [XMsrData] -> Map Loc OctaveLine
computeOctShift xMsrData = M.mapMaybeWithKey toLine occurrences
  where
    occurrences :: Map Loc Int
    occurrences = M.fromList . mapMaybe maybeOctShift . lMapToList $ xMsrData
    warning loc = printf ("Warning: in XML, octave-shift at %s is not " ++
                  "followed by octave-shift stop") (simpleShowLoc loc)
    toLine :: Loc -> Int -> Maybe OctaveLine
    toLine _   0 = Nothing
    toLine loc s = case M.lookupGT loc occurrences of
      Nothing         -> warning loc `trace` Nothing
      Just (endLoc,0) -> Just $ OctaveLine s endLoc
      _               -> warning loc `trace` Nothing


maybeOctShift :: (Loc,XMsrData) -> Maybe (Loc,Int)
maybeOctShift (loc,XMDDirection (XDOctaveShift s) _ _ _) = Just (loc,s)
maybeOctShift _ = Nothing


processOctaveLines :: Map Loc OctaveLine -> Map Loc (Map Int Chord) ->
                      Map Loc (Map Int Chord)
processOctaveLines lines = mapOverNotes g
  where
    g :: Loc -> Note -> Note
    g loc note = case M.lookupLE loc lines of 
      Nothing -> note
      Just (_,OctaveLine s end) | loc < end -> octShiftNote s note
                                | otherwise -> note


octShiftNote :: Int -> Note -> Note
octShiftNote n note@Note{ nPitch = nPitchIn } = note { nPitch = nPitchOut }
  where
    nPitchOut = octShiftPitch n nPitchIn


-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--                  compute pedal events


computePedalEvts :: Map Loc [XMsrData] -> Map Loc PedalEvt
computePedalEvts = fixPedalEvts . lMapMaybe g
  where
    g :: XMsrData -> Maybe PedalEvt
    g (XMDDirection (XDPedal type_ hasLine) _ _ _ _) = Just type_
    g _ = Nothing


-- look for bugs or weird things in the Sibelius XML pedal event export
--
-- First enforce there is only one pedal event at each location, except for
-- the following situations in which there are two pedal events:
-- 
--  (1) two pedal start at same Loc:         keep only one
--  (2) two pedal change at same Loc:        keep only one
fixPedalEvts :: Map Loc [PedalEvt] -> Map Loc PedalEvt
fixPedalEvts = M.map g
  where
    g :: [PedalEvt] -> PedalEvt
    g evts = case evts of
      [x]                        -> x
      [PedalStart , PedalStart ] -> PedalStart
      [PedalChange, PedalChange] -> PedalChange
      

----------------------------------------------------------------------
----------------------------------------------------------------------
--               compute symbols


computeSymbols :: Map Loc [XMsrData] -> Map Loc [Symbol]
computeSymbols = lMapMaybe g
  where
    g :: XMsrData -> Maybe Symbol
    g (XMDDirection (XDOtherDirection s) _ mVoice _ _) = case mVoice of
      Just v -> Just $ Symbol s v
    g _ = Nothing


----------------------------------------------------------------------
----------------------------------------------------------------------
--                compute text


computeText :: Map Loc [XMsrData] -> Map Loc [Text]
computeText = listToLMap . mapMaybe g . lMapToList
  where
    g (loc,x) = fmap (loc,) $ f x
    f (XMDDirection (XDWords s defy) _ _ _ _) = Just $ TechniqueText s
    f _ = Nothing


----------------------------------------------------------------------
----------------------------------------------------------------------



-- SlurData: <flag if any unnumbered slur starts here>
--           <the numbers of any numbered slur starts here>
--           <flag if any unnumbered slur stops here>
--           <the numbers of any numbered slur stops here>
data SlurData = SlurData Bool (Set Int) Bool (Set Int)
              deriving (Show)



computeSlurs :: Map Loc [XMsrData] -> Map Loc Loc
computeSlurs xs = M.fromList $ mapMaybe g ts
  where
    m = computeSlurs2 xs
    ts = L.tails $ M.toAscList m
    -- g takes a list of SlurData and searches it for a stop slur. 
    g :: [(Loc,SlurData)] -> Maybe (Loc,Loc)
    g [] = Nothing
    g ((loc,SlurData f1 s1 _ _):remain)
        | f1 && S.null s1 =
            (loc,) <$> seekStopUnnumbered remain
        | f1 && not (S.null s1) = throwMine "XmlToScore:computeSlurs"
        | not f1 && length s1 == 1 =
            (loc,) <$> seekStopNumbered (S.elemAt 0 s1) remain
        | otherwise = Nothing
        where
          -- seeking the stop of the slur (an unnebered type)
          seekStopUnnumbered :: [(Loc,SlurData)] -> Maybe Loc
          seekStopUnnumbered zs = fst <$> L.find (isStopUnnumbered . snd) zs
          isStopUnnumbered (SlurData _ _ f2 _) = f2

          seekStopNumbered n zs =  fst <$> L.find (isStopNumbered n . snd) zs
          isStopNumbered n (SlurData _ _ _ s2) = S.member n s2
      

computeSlurs2 :: Map Loc [XMsrData] -> Map Loc SlurData
computeSlurs2 = M.mapMaybe g
  where
    g ::[XMsrData] -> Maybe SlurData
    g xs = f d
      where
        ys = concatMap maybeSlurs xs
        isStartNoNum  (flag,Nothing) = flag
        isStartNoNum  (_   ,Just _ ) = False
        isStopNoNum   (flag,Nothing) = not flag
        isStopNoNum   (_   ,Just _ ) = False
        maybeStartNum (flag,Just n ) = if flag then Just n else Nothing
        maybeStartNum (_   ,Nothing) = Nothing
        maybeStopNum  (flag,Just n ) = if flag then Nothing else Just n
        maybeStopNum  (flag,Nothing) = Nothing
        d = SlurData (any isStartNoNum ys)
                     (S.fromList $ mapMaybe maybeStartNum ys)
                     (any isStopNoNum ys)
                     (S.fromList $ mapMaybe maybeStopNum ys)
        f s@(SlurData f1 s1 f2 s2) | not f1 && not f2 &&
                                     S.null s1 && S.null s2 = Nothing
                                   | otherwise = Just s


maybeSlurs :: XMsrData -> [(Bool,Maybe Int)]
maybeSlurs (XMDNote (XNNote _ _ _ _ _ _ _ _ notations _ _) _) =
    mapMaybe maybeNSlur notations
  where
    maybeNSlur :: XNotation -> Maybe (Bool,Maybe Int)
    maybeNSlur (XNSlur s mi) | s == "start" = Just (True ,mi)
                             | s == "stop"  = Just (False,mi)
    maybeNSlur _ = Nothing
maybeSlurs _ = []

maybeSlur (XMDNote (XNNote _ _ _ _ _ _ _ _ notations _ _) _) =
  not $ null [x | XNSlur x _ <- notations]
isSlur _ = False




----------------------------------------------------------------------
----------------------------------------------------------------------
--                    functions for processing Marks


metAndSymbolMarks :: Map Loc [XMsrData] -> Map Loc [MarkD]
metAndSymbolMarks msrData = 
  M.unionWith (++) (lMapMaybeWithKey maybeMetMark msrData) 
                   (lMapMaybe        maybeSymbol  msrData)


-- 
maybeMetMark :: Loc -> XMsrData -> Maybe MarkD
maybeMetMark loc (XMDDirection (XDMetronome beatType bpm) _ _ _ _) =
  case beatType of
    "quarter" -> Just $ SetTempo Nothing (fromIntegral bpm) False
    _         -> throwMine $ printf ("don't know metronome marking of " ++
                 "beat type %s at %s") beatType (simpleShowLoc loc)
maybeMetMark _ _ = Nothing
  

maybeSymbol :: XMsrData -> Maybe MarkD
maybeSymbol (XMDDirection (XDOtherDirection s) _ mVoice _ _) = 
  case mVoice of
    Just v -> Just $ SymbolMark s v
maybeSymbol _ = Nothing


----------------------------------------------------------------------
----------------------------------------------------------------------
--                  compute dynamics


computeDynamics :: [Int] -> Map Loc [XMsrData] -> [Map Loc Dynamic]
computeDynamics staffNums xmsr = map doScoreStaff staffNums
  where
    doScoreStaff :: Int -> Map Loc Dynamic
    doScoreStaff vn = computeDynamicsOneScoreStaff xs2
      where
        xs2 = filterXMsrDataByStaff xmsr vn
-- filterXMsrDataByStaff :: Map Loc [XMsrData] -> Int -> Map Loc [XMsrData]
    

-- 
computeDynamicsOneScoreStaff :: Map Loc [XMsrData] -> Map Loc Dynamic
computeDynamicsOneScoreStaff xmsr = d3
  where
    -- d1 :: Map Loc [Dynamic]
    d1 :: Map Loc [Maybe Dynamic]
    d1 = M.mapWithKey (\k vs -> map (f k) vs) xmsr
    d2 :: Map Loc [Dynamic] -- could have some null elems
    d2 = M.map catMaybes d1
    d3 :: Map Loc Dynamic
    d3 = M.mapMaybeWithKey step d2
    step :: Loc -> [Dynamic] -> Maybe Dynamic
    step loc ds = case ds of 
      []  -> Nothing
      [x] -> Just x
      _   -> throwMine $ printf "Multiple dynamics on same staff at %s" (simpleShowLoc loc)



    f :: Loc -> XMsrData -> Maybe Dynamic
    f loc (XMDDirection (XDDynamics s) mOffset _ mStaff _)
      = case toDyn s (case mStaff of {Just v -> v}) of
         Nothing -> printf ("Warning, unknown dynamic string %s" ++
                    " at %s") s (simpleShowLoc loc) `trace` Nothing
         Just x -> Just x
    f _ _ = Nothing


----------------------------------------------------------------------
----------------------------------------------------------------------
--               compute haipins


-- computeHairpins
--
-- Params:
--   Map Loc [XMsrData] -- all msrdata for this staff (part)
-- - we are interested in the Wedge XMsrData
-- - strategy is to filter this by score staff number and generate separate Map Loc [XMsrData]
--   for each score staff
-- - then call computeHairpinsOneScoreStaff for each Map Loc [XMsrData]
computeHairpins :: [Int] -> Map Loc [XMsrData] -> [Map Loc Hairpin]
computeHairpins staffNums xmsrs = map computeHairpinsOneScoreStaff d1
  where
    -- filterOne1 :: Int -> Map Loc [XMsrData]
    -- filterOne1 staffNum = M.map catMaybes . M.map (map (isOnStaff staffNum)) $ xmsrs
    -- -- okay we need to sort
    d1 :: [Map Loc [XMsrData]]
    d1 = map (filterXMsrDataByStaff xmsrs) staffNums

filterXMsrDataByStaff :: Map Loc [XMsrData] -> Int -> Map Loc [XMsrData]
filterXMsrDataByStaff xmsrs staffNum = M.map catMaybes . M.map (map (isOnStaff staffNum)) $ xmsrs



isOnStaff :: Int -> XMsrData -> Maybe XMsrData
isOnStaff x d = case d of
  a@(XMDDirection _ _ _ mStaff _) -> case mStaff of
    Just i | i == x    -> Just a
           | otherwise -> Nothing
    Nothing            -> Nothing
  a@(XMDNote n _) -> case xnStaff n of
    Just i | i == x    -> Just a
           | otherwise -> Nothing
    Nothing            -> Nothing
  _                    -> Nothing
    

computeHairpinsOneScoreStaff :: Map Loc [XMsrData] -> Map Loc Hairpin
computeHairpinsOneScoreStaff = M.fromList . mapMaybe pairUp . L.tails .
                  mapMaybe toWedge . lMapToList
  where
    toWedge (loc,XMDDirection (XDWedge t) _ _ mStaff _) = 
      Just (loc,t,case mStaff of {Just s -> s})
    toWedge _ = Nothing
    pairUp ((loc1,WedgeCresc,staffN):(loc2,WedgeStop,_):_) = 
        Just (loc1, Hairpin Crescendo  loc2 staffN)
    pairUp ((loc1,WedgeDim,staffN)  :(loc2,WedgeStop,_):_) = 
        Just (loc1, Hairpin Diminuendo loc2 staffN)
    pairUp _ = Nothing

----------------------------------------------------------------------
----------------------------------------------------------------------
--               compute chords


-- arguments
--   Map Int IXMsrInfo
--   Map Loc [XMsrData] : here loc is the location of the XMsrData
-- output
--   Map Loc (Map Int PrelimChord)
--
-- algorithm: feeds through
--   xMsrDataToNote: filters out just the XMDNote (would include grace notes)
--   groupNotesByVoice
--   noteMapToChordMap
computeChords :: Map Int IXMsrInfo -> Map Loc [XMsrData] -> 
                 Map Loc (Map Int PrelimChord)
computeChords msrInfo =
  M.mapWithKey (noteMapToChordMap msrInfo) . M.map groupNotesByVoice
  . lMapMaybe xMsrDataToNote

{-
sibeliusVoiceNumbers :: Bool -> Map Loc [XNote] -> Map Loc [XNote]
sibeliusVoiceNumbers isSib m 
  | isSib     = M.map (map replace) m
  | otherwise = m
  where
    -- here we need to replace every note voice number by 4 times staff number
    -- + voice number
    replace :: XNote -> XNote
    replace n@(XNNote {}) = n {xnVoice = Just ((st-1)*4 + v)}
      where
        v = case xnVoice n of {Just v -> v; Nothing -> error "foo"}
        st = case xnStaff n of {Just s -> s; Nothing -> error "foo"}
    replace _ = error "foo"
-}


xMsrDataToNote :: XMsrData -> Maybe XNote
xMsrDataToNote (XMDNote n@XNNote{} _) 
  | isJust $ xnIsGrace n  = Nothing
  | otherwise             = Just n
xMsrDataToNote _ = Nothing


groupNotesByVoice :: [XNote] -> Map Int [XNote]
groupNotesByVoice = listToLMap . map (\n -> (getVoice n,n))
  where getVoice n = case XD.xnVoice n of {Just v -> v}

-- DATA
--
--
-- parameters:

-- TNOTE
-- noteMapToChordMapTNote :: Map Int IXMsrInfo -> Loc -> Map Int [TNote]
--   -> Map Int PrelimChord


noteMapToChordMap :: Map Int IXMsrInfo -> Loc -> Map Int [XNote] -> 
                     Map Int PrelimChord
noteMapToChordMap msrInfo chordBegin = 
  M.map (notesToChord chordBegin msrInfo)

-- DATA
--
-- data XNote = rest, note, grace, [XNotation]. nothing about timing
--
-- paramters
--    Loc: chord begin. used to compute end loc but why not used in PrelimChord?
--    Map Int IXMsrInfor
--    [XNote]
--
-- algorithm:
--   uses toNote to convert an XNote to a Note
--   computes chord modifier data. seems to be filtering [XNotation] to
--     find things relevant to chords

notesToChord :: Loc -> Map Int IXMsrInfo -> [XNote] -> PrelimChord
notesToChord = error "foo"
{-
notesToChord chordBegin msrInfo notesIn = 
    PrelimChord endingLoc modifierData notesOut
  where
  firstNote:_ = notesIn
  endingLoc = computeEndLoc msrInfo chordBegin (XD.xnDuration firstNote)
  modifierData = S.fromList . concatMap getChordModifiers $ notesIn
  -- notesOut = M.fromList . zip [1..] . map toNote $ notesIn
  notesOut = map toNote notesIn
-}


-- DATA
-- data XNote = rest, note, grace. nothing about timing
--
-- algorithm:
--    mainly computes midi pitch
toNote :: XNote -> Note
toNote XNNote { XD.xnPitch = XPitch stepString alter octave
              , XD.xnTieStart = isTied } =
  Note (Pitch midiPitch step alter octave) isTied (Loc 0 0) 
    NormalHead
  where
    step = case lookup stepString [ ("C",0),("D",1),("E",2),("F",3)
                                  , ("G",4),("A",5),("B",6)] of 
      Just x -> x
    pitchClass = 
      case lookup stepString [("C",0),("D",2),("E",4),("F",5),("G",7)
                             ,("A",9),("B",11)] of
        Just x -> x
    midiPitch = (octave+1) * 12 + pitchClass + alter


-- computeEndLoc
--
-- Map Int IXMsrInfo
-- Loc : begin loc of a note
-- Int : duration of the note in number of divisions per quarter
--
{-
moved to XmlToScore_ties
computeEndLoc :: Map Int IXMsrInfo -> Loc -> Int -> Loc
computeEndLoc xmis (Loc locMsr locBeat) dur = case M.lookup locMsr xmis of
  Just (IXMsrInfo dpq numer denom)
    -- exhausted case means number of divs put end loc past beat 1 of next msr
    | e == fromIntegral numer + 1 -> Loc (locMsr+1) 1
    | e <  fromIntegral numer + 1 -> Loc locMsr e
    where
      e = locBeat + (fromIntegral denom / 4) *
          (fromIntegral dur / fromIntegral dpq)
-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--                      ties

-- computeChordEndsMap
--
--  m :: Map Loc (Map Int PrelimChord)
--  output: Map <end loc> <every chord ending there>
--     <every chord ending there> = [(Loc,Int)] = [(start loc,voice)]
computeChordEndsMap :: Map Loc (Map Int PrelimChord) -> Map Loc [(Loc,Int)]
computeChordEndsMap m = listToLMap $ concatMap g $ M.toList m
  where
    -- maps one entry in m representing a 'located' and 'voiced' chord
    --   to [(<end Loc>,(<begin Loc,voice number)]
    g :: (Loc,(Map Int PrelimChord)) -> [(Loc,(Loc,Int))]
    g (atLoc,m) = map (\(i,p) -> (prcEndLoc p,(atLoc,i))) $ M.toList m



{-
-- isTrill
--
-- This will compute the pitch of the "upper" trill note if there
-- is a trill mark here
--
--    it also p
isTrill :: Map Loc [MarkD] -> Loc -> Int -> Maybe Int
isTrill syms atLoc vn =
    M.lookup atLoc syms >>= listToMaybe . mapMaybe (mTrill vn)
  

mTrill :: Int -> MarkD -> Maybe Int
mTrill vn (SymbolMark s vTest) | vTest /= vn = Nothing
                               | otherwise = case s of
                                   "trill-natural" -> Just 0
                                   "trill-flat"    -> Just (-1)
                                   "trill-sharp"   -> Just 1
                                   "Trill"         -> Just 0
                                   _               -> Nothing

mTrill _ _ = Nothing
-}
----------------------------------------------------------------------
----------------------------------------------------------------------
--              Computing blank measures


computeUsedMsrs :: Set Int -> Array Int Bool
computeUsedMsrs s = listArray (1,maxMsr) (repeat False) //
                    zip (S.toList s) (repeat True)
  where
    maxMsr = maybe 1 fst (S.maxView s)



----------------------------------------------------------------------
----------------------------------------------------------------------

{-
findDoubTremStart :: PrelimChord -> Maybe Int
findDoubTremStart chord = 
  listToMaybe [n | DoubTremStart n <- S.toList $ prcModifiers chord]


findDoubTremStop :: PrelimChord -> Maybe Int
findDoubTremStop chord = 
  listToMaybe [n | DoubTremStop n <- S.toList $ prcModifiers chord]
-}
----------------------------------------------------------------------
----------------------------------------------------------------------
--                   compute Markers


computeMarkers :: Map Loc [MarkD] -> Markers
-- computeMarkers marks = Markers (s isW) (s isCD) (s isDC) (s isCaret)
computeMarkers marks = Markers (s isW) (s (const False)) (s (const False))
                               (s (const False))
  where
    s pred = S.fromList . map fst . filter f $ M.toList marks
      where
        f (loc,ms) = any pred ms


isW :: MarkD -> Bool
isW W = True
isW _ = False



xmlToScoreTest :: XScore -> [(String,Map Loc (Map Int PrelimChord))]
xmlToScoreTest xs = map (f2 . f1) staves
  where
    staves = xmlToScoreTest2 xs
    xmlStaves :: Map String (Map Loc [XMsrData])
    (imix,xmlStaves) = computeXmlStaves xs
    staveWords = M.map (lMapMaybe maybeWord) xmlStaves
    allMarks' = computeWordMarks stavesWords
    f1 :: (String,[TNote]) -> (String,Map Loc (Map Int [TNote]))
    f1 (s,tns) = (s,tNotesToVoicesLocs tns)
    f2 :: (String,Map Loc (Map Int [TNote])) -> 
          (String,Map Loc (Map Int PrelimChord))
    f2 (s,tns) = (s, tNotesToPrelimChords tns)
    stavesWords :: Map String (Map Loc [WordDirection])
    stavesWords = M.map (lMapMaybe maybeWord) xmlStaves
    
    f3 :: (String,Map Loc (Map Int PrelimChord)) ->
          (String,Map Loc (Map Int Chord))
    f3 (s,pcs) = error "foo"


xmlToScoreTest2 :: XScore -> [(String,[TNote])]
xmlToScoreTest2 xs = m3
  where
    imix :: Map Int IXMsrInfo
    m :: Map String (Map Loc [XMsrData])
    (imix,m) = computeXmlStaves xs
    m2 :: (String,Map Loc [XMsrData]) -> (String,[TNote])
    m2 (s,m_) = (s, xmlToScoreTestStaff imix m_)
    m3 :: [(String,[TNote])]
    m3 = map m2 $ M.toAscList m


xmlToScoreTest3 :: XScore -> [(String,[TNote])]
xmlToScoreTest3 xs = map m2 $ M.toAscList m
  where
    (imix,m) = computeXmlStaves xs
    m2 (s,m_) = (s, xmlToScoreTestStaff imix m_)

    

xmlToScoreTestStaff :: Map Int IXMsrInfo -> Map Loc [XMsrData] -> 
  [TNote]
xmlToScoreTestStaff imix m = d3
  where
    d1 :: [(Loc,[XMsrData])]
    d1 = M.toAscList m
    d2 :: [(Loc,XMsrData)]
    d2 = tmp1 d1
    d3 :: [TNote]
    d3 = doTiesXMsrData imix d2


-- doTiesXMsrData :: Map Int IXMsrInfo -> [(Loc,XMsrData)] ->
--   [TNote]

-- computeXmlStaves :: XScore -> ( Map Int IXMsrInfo
--                               , Map String (Map Loc [XMsrData]) )

tmp1 :: [(a,[b])] -> [(a,b)]
tmp1 = concatMap g
  where
    g :: (a,[b]) -> [(a,b)]
    g (x,ys) = map (x,) ys

-- doTiesXMsrData :: Map Int IXMsrInfo -> [(Loc,XMsrData)] ->
--   [TNote]