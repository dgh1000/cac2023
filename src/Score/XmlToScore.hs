{-# LANGUAGE TupleSections, ScopedTypeVariables #-}
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
import Score.XmlToScore_ties
import Common
import Util.Exception
import Util.Map
import Common.CommonUtil
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
    staves       = M.mapWithKey (computeStaff xMsrInfo timeSigs) $ xmlStaves
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
--                   mark computations

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
          , stMaxTrueEnd     = computeMaxTrueEnd chords
          , stUsedMsrs       = S.fromList . map msrNum . M.keys $ prelimChords
          , stSlurs          = computeSlurs xmlStaff
          , stChords         = chords 
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
    chords       = prelimChord2Chord metSym $
                   processTies chordEndsMap prelimChords
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
  M.mapWithKey (noteMapToChordMap msrInfo) . M.map groupNotesByVoice . 
    lMapMaybe xMsrDataToNote

-- data PrelimChord = PrelimChord
--   { prcEndLoc     :: Loc
--   , prcModifiers  :: Set ChordModifier
--   , prcNotes      :: [Note]
--   }


-- should we not convert grace notes to note
-- XNote has a duration, voice, staff, pitce,  tie, notations, notehead
-- BUT NOT A LOC

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
noteMapToChordMapTNote :: Map Int IXMsrInfo -> Loc -> Map Int [TNote]
  -> Map Int PrelimChord

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
notesToChord chordBegin msrInfo notesIn = 
    PrelimChord endingLoc modifierData notesOut
  where
  firstNote:_ = notesIn
  endingLoc = computeEndLoc msrInfo chordBegin (XD.xnDuration firstNote)
  modifierData = S.fromList . concatMap getChordModifiers $ notesIn
  -- notesOut = M.fromList . zip [1..] . map toNote $ notesIn
  notesOut = map toNote notesIn


getChordModifiers :: XNote -> [ChordModifier]
getChordModifiers n@XNNote {XD.xnNotations=notations} = concatMap g notations
  where
  g :: XNotation -> [ChordModifier]
  g (XNArticulations arts) = concatMap artToMod arts
  g (XNOrnaments orns)     = mapMaybe ornToMod orns
  g XNFermata              = [Fermata]
  g XNArpeggiate           = [Arpeggiate]
  g (XNTechnical techs)    = map techToMod techs
  g (XNSlur _ _)           = []
  artToMod XAStaccato       = [Staccato]
  artToMod XAStaccatissimo  = [Staccatissimo]
  artToMod XAAccent         = [Accent]
  artToMod XAStrongAccent   = [StrongAccent]
  artToMod XATenuto         = [Tenuto]
  artToMod XADetachedLegato = [Staccato,Tenuto]
  ornToMod (Tremolo type_ nBars) = Just $ case type_ of
    TremoloSingle  -> SingTrem      nBars
    TremoloStart   -> DoubTremStart nBars
    TremoloStop    -> DoubTremStop  nBars
  ornToMod _ = Nothing
  techToMod XTOpenString = OpenString
  techToMod XTDownBow    = DownBow
  techToMod XTUpBow      = UpBow

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



-- processTies
--
-- Set true end of tied notes, and eliminate notes and chords that are
-- at the far end of ties.
--

-- processTies
--
-- Strategy for handling ties. In tie chain, first note end time is 
--   extended to end of chain, and other chords are removed. 
--
-- chordEnds :: Map Loc [(Loc,Int)] ::
--    Map <end> <all beginnings/voices with that end>
-- staff: Map Loc (Map Int PrelimChord)
-- output:: Map Loc (Map Int PrelimChord) but with 
-- 
processTies :: Map Loc [(Loc,Int)] -> Map Loc (Map Int PrelimChord) ->
               (Map Loc (Map Int PrelimChord))
processTies chordEnds staff = M.mapMaybeWithKey processLoc staff
  where
    -- processLoc
    --
    --   Loc: a begin Loc
    --   Map Int PrelimChord: all notes at that begin
    --   Maybe (Map Int PrelimChord): result after removing notes
    --     that are "tie middles" and (Maybe?) adjusting endloc or 
    --     trueendloc of notes that remain
    processLoc :: Loc -> Map Int PrelimChord -> (Maybe (Map Int PrelimChord))
    processLoc loc = mmwk processChord
      where
        -- processChord
        --
        -- Int :: voice number
        -- PrelimChord 
        -- Maybe PrelimChord: whatever notes are left, Nothing if none
        processChord :: Int -> PrelimChord -> Maybe PrelimChord
        processChord vn ch@(PrelimChord end _ ns) =
          let set_NSingles ns = ch {prcNotes=ns}
              -- doTieNote: decide for one note whether to keep it or
              -- remove it and adjust its end if necessary
              --   
              f :: Note -> Maybe [Note]
              f note = (:[]) <$> doTieNote chordEnds staff loc vn end note
          in set_NSingles <$> (mconcat $ map f ns)

                 
mmwk :: Ord k => (k -> a -> Maybe b) -> Map k a -> Maybe (Map k b)
mmwk f m | M.null m2 = Nothing
         | otherwise = Just m2
  where m2 = M.mapMaybeWithKey f m


--
-- Transform a note based on tie rules.
--
--   1. If this note is precedded by a tie, get rid of it (return Nothing)
--
--   2. Otherwsie, try to compute true end of it.
--
-- How we tell if a note, of 'atLoc' and 'vn', is preceeded by a tie?
--
--   Lookup 'atLoc' in the chord end map: that will give us [(Loc,Int)]
--
--   Those are a bunch of start locs of chords and voice numbers in the staff
--   data. Look up each of those chords, and look at each of those notes for a
--   tied note of the same pitch in any voice
--
--
--
--   True end of noteIn: (needed going into this, the chord end of the note
--                        and the voice number)
--
--   1. if the note has no tie, then the chord end is the true end
--
--   2. if the note is tied, then we begin THE HUNT for a note that follows it
--
--      a. take chord end of noteIn, and look up all chords at that Loc, with
--         or withou matching voice number
--
--      b. put all chords in order with the ones with the matching number at
--         the head of the line
--
--      c. search for first note that matches in pitch. note it's chord end
--         loc and voice number
--
--      d. go to 1
doTieNote :: Map Loc [(Loc,Int)] -> Map Loc (Map Int PrelimChord) ->
             Loc -> Int -> Loc -> Note -> Maybe Note
doTieNote chordEnds staff atLoc vnIn end note
  | isEndTie  = Nothing
  | otherwise = Just note {nTrueEnd = findTrueEnd staff end vnIn note}
  where
    -- m1: chords that end at 'atLoc' if any (identified by their beginning
    --   and voice number)
    m1 :: Maybe [(Loc,Int)]
    m1 = M.lookup atLoc chordEnds
    -- m2: m1 put in order so that vnIn comes first
    m2 :: Maybe [(Loc,Int)]
    m2 = myOrdering2 vnIn <$> m1
    -- f1:
    --  (Loc,Int) is <chord begin loc>, <voice number>
    --  This looks up that chord in 'staff' as assumes there is one
    --  (else will be a non-exhaustive case error)
    f1 :: (Loc,Int) -> PrelimChord
    f1 (l,i) = case M.lookup l staff >>= M.lookup i of
      Just c -> c
    -- m3: transformation of m2 into prelim chorods themselves
    m3 :: Maybe [PrelimChord]
    m3 = map f1 <$> m2
    -- pitchMatch: retrieves midi pitch of the 'note' we are processing
    pitchMatch = midiPitch $ nPitch note
    -- f2: takes a PrelimChord and returns list of notes matching pitch match
    --   (if I'm not mistaken there shouldn't be more than one)
    f2 :: PrelimChord -> [Note]
    f2 (PrelimChord _ _ ns) = filter ((==pitchMatch) . midiPitch . nPitch) ns
    -- m4: mapping f2 to the inside of m3, retrieving all matching notes
    --   
    --   could there be more than one? not likely since usually chords 
    --   in different voices will not have overlapping notes
    m4 :: Maybe [Note]
    m4 = concatMap f2 <$> m3
    -- isEndTie: are any of those matching notes tie begins?
    isEndTie = case m4 of
      Just ns -> any nIsTied ns
      Nothing -> False


myOrdering :: Eq a => a -> [(a,b)] -> [(a,b)]
myOrdering x list = xs++ys
  where
    (xs,ys) = L.partition ((==x) . fst) list


myOrdering2 :: Eq b => b -> [(a,b)] -> [(a,b)]
myOrdering2 x list = xs++ys
  where
    (xs,ys) = L.partition ((==x) . snd) list


-- findTrueEnd
--
-- Map Loc (Map Int PrelimChord) :: Staff
-- Loc :: end Loc of 'noteIn'
-- Int :: voice of 'noteIn'
-- Note
-- output Loc: true end, following it all through all the ties
--
findTrueEnd :: Map Loc (Map Int PrelimChord) -> Loc -> Int -> Note -> Loc
findTrueEnd staff end vn noteIn 
  | not (nIsTied noteIn) = end
  | otherwise = case maybeNote of
      -- call findTrueEnd recursively: with staff, endOut
      Nothing -> printf "Warning, no matching note follows tie at %s"
                 (showLoc2 end) `trace` end
      Just ((vnOut,endOut),n) -> findTrueEnd staff endOut vnOut n
  where
    midiIn = midiPitch $ nPitch noteIn
    -- a :: chords that appear in staff at 'end' 
    a :: Maybe (Map Int PrelimChord)
    a = M.lookup end staff
    -- if that finds something, 'b' is a list of those chords ordered with 
    -- the preferred voice first
    b :: Maybe [(Int,PrelimChord)]
    b = a >>= return . myOrdering vn . M.toList
    -- 'c' is each chord broken out into its notes, with voice number and end
    -- Loc prepended
    c :: Maybe [((Int,Loc),Note)]
    c = b >>= return . concatMap (\(v,c) ->
                                   map ((v,prcEndLoc c),) (prcNotes c))
    -- here we look for the note at 'end' matching pitch
    maybeNote :: Maybe ((Int,Loc),Note)
    maybeNote = c >>= L.find (\(_,n) -> midiIn == (midiPitch $ nPitch n))
   

----------------------------------------------------------------------
----------------------------------------------------------------------
--                  double tremolos

-- to convert PrelimChord to Chord, doing trills at same time
--
-- basically map through each PrelimChord, changing to Chord or maybe
-- eliminating it
--
-- what do we do to decide what should be done with a PrelimChord?
--
--   first see if it's marked as a doub tremolo start
--
--     if so, we need to find the chord that ends it, grab those pitches and
--     include them.
--
--   or consider if it's a doub trem stop. then remove it
--
--   if none of the above, then check if it's a trill, and evaluate pitches if
--   so


-- we creat a function that takes a Loc and a sub-prelim chord, and converts
-- it to a sub-chord. voila! now just run this over everything and we'll be
-- done.
--
-- problem is broken down by mapping with key over sub-chords at dififerent
-- Locs. so the setup is ~~~~ you have a Loc ~~~~> and you get the submap,
-- Maybe
--
--    this is function pc2c_loc. now we can pass that over ever key in our big
--    map
--
--    so pc2c_loc has to work on the Ints of sub-maps with the help of
--
--       pc2c_vn: here now that we receiving the vn as an input, we can
--       finally full idified ctbwo: chord ctobwo, or called c1 donw there.
--
--          easy cases: if it's a marked double tremolo stop, don't sound it,
--          remove it from the score.
--
--          next easy case: set up a Trill


prelimChord2Chord :: Map Loc [MarkD] -> Map Loc (Map Int PrelimChord) ->
                     Map Loc (Map Int Chord)
prelimChord2Chord symbols staff = M.mapMaybeWithKey pc2c_loc staff
  where
    -- what does this need to do its job? 'staff' to look up doub
    -- tremolos. it's going to take a chord with some notes including a true
    -- end, and then look at a certain place for a matching chord with a doub
    -- trem stop setting. same voice
    pc2c_loc :: Loc -> (Map Int PrelimChord) -> Maybe (Map Int Chord)
    pc2c_loc atLoc = mmwk pc2c_vn
      where
        pc2c_vn :: Int -> PrelimChord -> Maybe Chord
        pc2c_vn vn c1
           | isJust $ findDoubTremStart c1 = Just $
                                             pc2c_dt_case staff atLoc vn c1

           | isJust $ findDoubTremStop c1  = Nothing
           | otherwise =
              case isTrill symbols atLoc vn of
               Nothing  -> Just $ pc2c_main_case c1
               Just trN -> pc2c_trill_case atLoc trN c1


pc2c_main_case :: PrelimChord -> Chord
pc2c_main_case (PrelimChord end mods ns) =
  -- GRACENOTES
  Chord end mods (NSingles $ M.fromList $ map (nmp &&& id) ns) []
  where
    nmp = midiPitch . nPitch
                     


pc2c_trill_case :: Loc -> Int -> PrelimChord -> Maybe Chord
pc2c_trill_case atLoc upperAlter c1 =               
   let msg = printf ("something wrong; a trill chord has more "++
             "than one note at %s") (showLoc2 atLoc)
       note1 = case prcNotes c1 of
                 [x]  -> x
                 _    -> throwMine msg
       pitch1 = nPitch note1
       midi1 = midiPitch pitch1
       (midi2,pitch2) = computeTrillPitch upperAlter pitch1
       trillDiff = midi2-midi1
       note2 = note1 {nPitch=pitch2}
       new1 = M.fromList [(midi1,note1)]
       new2 = M.fromList [(midi2,note2)]
   in Just $ Chord (prcEndLoc c1) (prcModifiers c1)
             (NTrill (TtnTrill trillDiff) new1 new2) []


mkNoteMap :: [Note] -> Map Int Note
mkNoteMap = M.fromList . map ((midiPitch . nPitch) &&& id)

pc2c_dt_case :: Map Loc (Map Int PrelimChord) -> Loc -> Int -> PrelimChord ->
                Chord
pc2c_dt_case staff atLoc vn c1@(PrelimChord end1 mods1 ns1) = case mns2 of
    Just (end2,ns2) -> Chord end2 mods1 (NTrill TtnTremolo (mkNoteMap ns1)
                                                        (mkNoteMap ns2) ) []
    Nothing -> printf "warning, no chord follows doub.tremolo at %s"
               (showLoc2 atLoc) `trace` Chord end1 mods1
                                        (NSingles (mkNoteMap ns1)) []
  where
    -- find the following chord if it exists, at end1
    a :: Maybe PrelimChord
    a = M.lookup end1 staff >>= M.lookup vn
    b :: PrelimChord -> Maybe (Loc,[Note])
    b c2 | isJust $ findDoubTremStop c2 = Just $ (prcEndLoc c2,prcNotes c2)
         | otherwise = Nothing
    -- c is the notes at double tremolo stop chord
    mns2 :: Maybe (Loc,[Note])
    mns2 = a >>= b
    

                                   
computeTrillPitch :: Int -> Pitch -> (Int,Pitch)
computeTrillPitch upperAlter (Pitch _ step1 alter1 octave1) = (newMidi,pitch2)
  where
    step2 = (step1+1) `mod` 7
    octave2 = if step1 == 6 then octave1+1 else octave1
    newMidi = stepAlterOctToMidi step2 upperAlter octave2
    pitch2 = Pitch newMidi step2 upperAlter octave2



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

findDoubTremStart :: PrelimChord -> Maybe Int
findDoubTremStart chord = 
  listToMaybe [n | DoubTremStart n <- S.toList $ prcModifiers chord]


findDoubTremStop :: PrelimChord -> Maybe Int
findDoubTremStop chord = 
  listToMaybe [n | DoubTremStop n <- S.toList $ prcModifiers chord]

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

{-
isCD EndCrescDescr = True
isCD _  = False


isDC EndDescrCresc = True
isDC _  = False


isCaret AdjustMarker = True
isCaret _ = False
-}

-- data XScore = XScore
--   { xPartInfos :: Map String XPartInfo -- map of id to part human-readable name
--   , xParts :: Map String XPart         
--   }
-- data XPart = XPart
--   { xpMsrs :: [XMsr]
--   }

-- xmlToScoreTest :: XScore -> Map String [TNote]
-- xmlToScoreTest xs = error "foo"
--   where
--     parts :: Map String XPart
--     parts = xParts xs  
--     toMsrs :: XPart -> [XMsr]
--     toMsrs = xpMsrs

xmlToScoreTest :: XScore -> [(String,[TNote])]
xmlToScoreTest xs = m3
  where
    imix :: Map Int IXMsrInfo
    m :: Map String (Map Loc [XMsrData])
    (imix,m) = computeXmlStaves xs
    m2 :: (String,Map Loc [XMsrData]) -> (String,[TNote])
    m2 (s,m_) = (s, xmlToScoreTestStaff imix m_)
    m3 :: [(String,[TNote])]
    m3 = map m2 $ M.toAscList m


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