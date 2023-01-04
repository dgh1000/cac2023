module XmlDoc.Process where

import qualified Data.Set as S
import qualified Data.Map as M
import qualified XmlDoc.XmlDocData as XD
import qualified Util.Map as UM
import Text.Printf
import Debug.Trace
import Control.Arrow
import Data.List
import Data.Maybe
import Data.Set(Set)
import Data.Map(Map)
import XmlDoc.XmlDocExport
import Common.CommonUtil
import Common
import Util.Exception



----------------------------------------------------------------------
----------------------------------------------------------------------
--        making type Map Loc [XMsrData]  for each part

-- DATA
-- data XScore (Map String XPartInfo) (Map String XPart)
-- data XPartInfo String (staff name)
-- data XPart [XMsr]
-- data XMsr Int (msr num) (Maybe XMsrAttr) [XMsrData]
-- data XMsrData: directions, notes, backup, forward
computeXmlStaves :: XScore -> ( Map Int IXMsrInfo
                              , Map String (Map Loc [XMsrData]) )
computeXmlStaves (XScore _ parts) 
  = (mis, M.map g $ parts)
  where
    mis = case M.toList parts of {(_,p):_ -> computeMsrInfos p}
    g :: XPart -> Map Loc [XMsrData]
    g (XPart msrs) = computeMsrDatas mis msrs

computeMsrInfos :: XPart -> Map Int IXMsrInfo
computeMsrInfos (XPart msrs) = 
  case scanl step (Nothing,Nothing,Nothing) . map transformMsr $ msrs of
    _:ys -> M.fromList . zip [1..] . map toMsrInfo $ ys
  where
    combine x Nothing = x
    combine _ (Just x) = Just x
    step (a, b, c) (XMsrAttr ma mb mc) 
      = (a `combine` ma, b `combine` mb, c `combine` mc)
    toMsrInfo (Just x,Just y,Just z) = IXMsrInfo x y z
    transformMsr :: XMsr -> XMsrAttr
    transformMsr (XMsr _ (Just msrAttr) _) = msrAttr
    transformMsr _ = XMsrAttr Nothing Nothing Nothing

data TimeState = TimeState Int Int (Maybe Int)  -- <prev time> <current time>
                                                -- <previous non-chord voice
                                                --  number>
-- tag: MUSESCORE , accumTime where time is tagged and chord voice is updated
-- accumTime:                  
accumTime :: [XMsrData] -> [(Int,XMsrData)]
accumTime = catMaybes . snd . mapAccumL f (TimeState 0 0 Nothing)
  where
    f :: TimeState -> XMsrData -> (TimeState,Maybe (Int,XMsrData))
    f (TimeState prevTime currTime pv) d = case d of
      XMDBackup d _ ->  (TimeState currTime (currTime-d) pv, Nothing)
      XMDForward d _-> (TimeState currTime (currTime+d) pv, Nothing)
      x@(XMDNote n _) 
        | xnChord n -> ( TimeState prevTime currTime pv
                       , Just (prevTime, updateChordVoice pv x) )
        | otherwise -> ( TimeState currTime (currTime + xnDuration n)
                                   (xnVoice n)
                       , Just (currTime, x) )
      x -> (TimeState prevTime currTime pv, Just (currTime, x))
      

-- tag: MUSESCORE - we modified this to mark chord voice whether
-- or not it was previously marked. It is marked to equal the first
-- argument to updateChordVoice and any marking already there is ignored
updateChordVoice :: Maybe Int -> XMsrData -> XMsrData
updateChordVoice (Just v) (XMDNote n ord) = case xnVoice n of
  Nothing -> XMDNote n { xnVoice = Just v } ord
  -- the following case wasn't there with Sibelius playback. It was enforcing
  -- that chord notes be unmarked in their voice numbers
  Just _  -> XMDNote n { xnVoice = Just v } ord

-- DATA
--
-- Map Int IXMsrInfo
-- [XMsr]. data XMsr msrNum (maybe msrAttr) [XMsrData]
--
-- parameters:
--   [XMsr]
--
-- algorithm: groups XMsrData by loc. this could be useful for extracting
--    grace notes
computeMsrDatas :: Map Int IXMsrInfo -> [XMsr] -> Map Loc [XMsrData]
computeMsrDatas mis 
  = assertDistinctStaffVoices . UM.listToLMap . map verifyNoteVoiceSet . 
    concatMap f
    
  where
    f :: XMsr -> [(Loc,XMsrData)]
    -- This will convert any XMsrData at a valid location to (Loc,XMsrData).
    -- Not sure why there might be invalid locations or if we have ever
    -- encountered that.
    -- accumTime :: [XMsrData] -> [(Int,XMsrData)]
    f m = mapMaybe maybeToLocMsrData $ accumTime $ XD.xmMsrDatas m
    -- f m = map (\(d,x) -> (toLoc (d + getOffset x),x)) . accumTime $ 
    --      XD.xmMsrDatas m
      where
     
        maybeToLocMsrData :: (Int,XMsrData) -> Maybe (Loc,XMsrData)
        maybeToLocMsrData (divs,xmd) = m2 (realDivs,xmd)
          where
            realDivs = divs + getOffset xmd
        m2 :: (Int,XMsrData) -> Maybe (Loc,XMsrData)
        m2 (divs,xmd) = 
          case divsToLoc mis (XD.xmMsrNum m) divs of
            Just l  -> Just (l,xmd)
            Nothing -> printf ("skipping XMsrData at an invalid location;" ++
                       " number of divisions is %d, \n%s") divs
                       (show xmd) `trace` Nothing


showAccumTime :: [(Int,XMsrData)] -> String
showAccumTime ms = unlines $ map g ms
  where
    g :: (Int,XMsrData) -> String
    g (n,xmd) = printf "%5d %s" n (show xmd)
    

getOffset :: XMsrData -> Int
getOffset (XMDDirection _ Nothing  _ _ _) = 0
getOffset (XMDDirection _ (Just i) _ _ _) = i
getOffset _                             = 0


{-
checkIfSlurs :: (Loc,XMsrData) -> (Loc,XMsrData)
checkIfSlurs (loc, d@(XMDNote (XNNote _ _ _ _ _ _ _ _ notations _)))
  | length [x | XNSlur x _ <- notations] > 0 =
      throwMine $ "can't handle slurs - there is one at " ++ (simpleShowLoc loc)
  | otherwise = (loc,d)
checkIfSlurs x = x
-}

-- verifyVoiceSet: looks like this is called only after fiddling with the voice on chords,
--     so it does expect to have a voice set
-- tag: MUSESCORE : this function no longer used. 
-- 
--   logic for Silbelius:
--
--   if XMDDIRECTION
--     if voice is set, it's fine (pass through)
--     if voice is not set, 
--       if XDDynmics throw an error
--       if XDWords
--         if there is a default-y attribute 
--            throw an error
--       any other constructor, or XDWords with no default-y attribute, it's fine
--
--  what can be learned: it looks like I'm expecting what Sibelius calls technique text
--  to be put in XDWords with positive default-y and a marked voice. I'm counting on
--  that voice to be there. 
--
--  Do I still need voice to be set for
--    - dynamics? no just need staff set
--    - words? no, all words will apply to all staves and voices in the part
--    - notes? yes, need that. write simpler routine

verifyVoiceSet :: (Loc,XMsrData) -> (Loc,XMsrData)
verifyVoiceSet t@(loc, XMDDirection xd _ v _ _)
  | isJust v = t
  | isNothing v = case xd of 
      XDDynamics _ -> throwMine $ printf ("XDDynamics missing voice number"++
        " at %s") (showLoc2 loc)
      XDWords _ (Just _) -> throwMine $ printf ("<words> at %s are in a " ++
        "bad state--they have a default-y attribute but no voice number")
        (showLoc2 loc)
      _ -> t
verifyVoiceSet t@(loc, XMDNote n _)
  | isJust . xnVoice $ n = t
  | otherwise = throwMine $ printf ("XMsrData of constructor XMDNote" ++
                " at %s is missing a voice number") (simpleShowLoc loc)
verifyVoiceSet x = x

verifyNoteVoiceSet :: (Loc,XMsrData) -> (Loc,XMsrData)
verifyNoteVoiceSet t@(loc, XMDNote n _)
  | isJust . xnVoice $ n = t
  | otherwise = throwMine $ printf ("XMsrData of constructor XMDNote" ++
                " at %s is missing a voice number") (simpleShowLoc loc)
verifyNoteVoiceSet x = x
  

-- divsToLoc
--
--   Given the location of an event at a certain number of time divisions
--   past the beginning of a measure, calculate the Loc. The divisions may
--   equal the entire duration of the measure, in which case the Loc is
--   computed as the first beat of the next measure. 
--
--   If the number of divisions would put the Loc beyond beat 1 of the next
--   measure, Nothing is returned.
divsToLoc :: Map Int IXMsrInfo -> Int -> Int -> Maybe Loc
divsToLoc mis msrNum divs
  | b == fromIntegral numer + 1 = Just $ Loc (msrNum+1) 1
  | b < fromIntegral numer + 1 = Just $ Loc msrNum b
  | otherwise = printf "divsToLoc: bad measure %d" msrNum `trace` Nothing
  where
    IXMsrInfo dpq numer denom = case M.lookup msrNum mis of
      Just x -> x
    b = 1 + (fromIntegral denom / 4) * (fromIntegral divs / fromIntegral dpq)

----------------------------------------------------------------------
----------------------------------------------------------------------
--          splitting staves and adjusting voices


-- splitMsrDataMap
--   Map <part id> <msr data by loc> -> Map <corresponds to meta name> <msr data by loc> 
--
--   for docs inside this function we call MLM = Map Loc [XMsrData]
-- MUSESCORE: in current form this function does nothing, as I intended when doing experiments
--            with MuseScore and piano staves. I'll remove any calls to it.
splitMsrDataMap :: Map String (Map Loc [XMsrData]) -> 
                   Map String (Map Loc [XMsrData])
splitMsrDataMap = M.fromList . concatMap g . M.toList
  where
    -- g seems to split piano grand staves into multiple Staff's with different names
    -- with -staff appended.
    --
    -- 'name' is human-readable name of part in XML
    g :: (String,Map Loc [XMsrData]) -> [(String,Map Loc [XMsrData])]
    g x = [x] -- tag: MUSESCORE
    {-
    g (name,m) = nameStaves . splitAndFixVoice $ m
      where
        -- nameStaves
        --   Called after splitting up parts by staff
        --
        --   If just one MLM in input, name it the XML human name
        nameStaves :: [(Int,Map Loc [XMsrData])] -> 
                      [(String,Map Loc [XMsrData])]
        nameStaves [(_,m)] = [(name,m)]
        --   If multiple MLM with different ints for staff
        nameStaves xs = map appendStaffName xs
        appendStaffName (staffNum,m) = (name ++ "-staff" ++ show staffNum, m)
    -}

-- assertDistinctStaffVoices
assertDistinctStaffVoices :: Map Loc [XMsrData] -> Map Loc [XMsrData]
assertDistinctStaffVoices mxs
  | allSetsCount == unionSetsCount = mxs
  | otherwise = 
      throwMine $ 
        "On some multiple-staff part, voice numbers on different staves overlap: " ++ show svSet
  where
    xs = concat $ M.elems mxs
    getStaffVoice :: XMsrData -> Maybe (Int,Int)
    getStaffVoice (XMDDirection xd _ (Just voice) (Just staff) _) =
      case xd of 
        (XDWedge _) -> Nothing
        _           -> Just (staff,voice)
    getStaffVoice (XMDNote n _) = 
      case (xnStaff n,xnVoice n) of (Just s, Just v) -> Just (s, v)
    getStaffVoice _ = Nothing
    stavesVoices :: [(Int,Int)]
    stavesVoices = mapMaybe getStaffVoice xs
    svSet :: Map Int (S.Set Int)
    svSet = M.fromListWith S.union $ map (\(x,y) -> (x,S.singleton y)) stavesVoices
    allSetsCount :: Int
    allSetsCount = sum . map S.size . M.elems $ svSet
    unionSetsCount :: Int
    unionSetsCount = S.size . S.unions . M.elems $ svSet
      

-- splitAndMarkVoice 
--
-- returns [(<staff num>, <XMsrData map>)]
splitAndFixVoice :: Map Loc [XMsrData] -> [(Int,Map Loc [XMsrData])]
splitAndFixVoice msrDatas
  | length staffNums == 1 = [(S.findMin staffNums, fixVoices True msrDatas)]
  | length staffNums > 1 =
    map (\i -> (i, fixVoices False . filterStaffNum msrDatas $ i))
        (S.toList staffNums)
  where
    -- Make a set containing all unique staff numbers Just n. Staff numbers
    -- that are 'Nothing' will trigger a case exhaustion
    staffNums = foldr step S.empty . map getStaff . concat . M.elems $ msrDatas
    step (Just n) s = S.insert n s
    -- case exhaustion here means a staff number was Nothing

-- filterStaffNum
--
filterStaffNum :: Map Loc [XMsrData] -> Int -> Map Loc [XMsrData]
filterStaffNum msrDatas num = UM.lMapMaybe go msrDatas
  where
    go :: XMsrData -> Maybe XMsrData
    go d = case getStaff d of
      -- Case exhaustion here means a staff number was Nothing
      Just s | s == num  -> Just d
             | otherwise -> Nothing



-- returns true if the voice number of this XMsrData item is "essential",
-- meaning that all of the following are true:
--
--   (1) it might have been altered by Sibelius during the xml export
--
--   (2) it's either a note voice, or it's important this voice match the
--   note voices
--
--   (3) it must be set when the algorithm is finished
hasEssentialVoice :: XMsrData -> Bool
hasEssentialVoice (XMDNote XNNote {} _)                    = True
hasEssentialVoice (XMDDirection XDWords {} _ mVoice _ _)   = isJust mVoice
hasEssentialVoice (XMDDirection XDDynamics {} _ _ _ _)     = True
hasEssentialVoice (XMDDirection XDOtherDirection {} _ _ _ _) = True
-- what remains is forward, backup, "other", XNRest, XDMetronome, XDWedge,
-- XDPedal
hasEssentialVoice _                                        = False

getVoice :: XMsrData -> Maybe Int
getVoice (XMDDirection _ _ v _ _) = v
getVoice (XMDNote n _)          = XD.xnVoice n
getVoice _                    = Nothing

getStaff :: XMsrData -> Maybe Int
getStaff (XMDDirection _ _ _ s _) = s
getStaff (XMDNote n _)          = XD.xnStaff n
getStaff _                    = Nothing

-- setVoice
--
-- Given an input voice V, modify the XMsrData so its voice will be Just V.
setVoice :: Int -> XMsrData -> XMsrData
setVoice v (XMDDirection x y _ s ord) = XMDDirection x y (Just v) s ord
setVoice v (XMDNote n ord) = XMDNote n { XD.xnVoice = Just v } ord
setVoice _ x = x

-- fixVoices 
-- 
-- Possibly change some of the voice numbers so that each staff starts with 1.
--
-- If 'wasSingleStaff' is True then enforce that the existing voices
-- numbers already started at 1.
--
-- Enforce that all essential voice numbers are not Nothing.
--
-- Input XMsrData is assumed to be from a single staff only.
fixVoices :: Bool -> Map Loc [XMsrData] -> Map Loc [XMsrData]
fixVoices wasSingleStaff msrDatas = case S.minView voiceNums of
  Just (minVoice,_) | wasSingleStaff && minVoice == 1 -> msrDatas
                    | not wasSingleStaff -> 
                        M.map (map $ normalizeVoice minVoice) msrDatas
  Nothing -> throwMine $ "Problem in Process.hs:fixVoices. Maybe no notes on "++
             "one of the staves?"
  where
    voiceNums = computeEssentialVoiceNumbers msrDatas


-- Compute essential voice numbers present in the staff and throw an
-- exception if any of them are Nothing.
computeEssentialVoiceNumbers :: Map Loc [XMsrData] -> Set Int
computeEssentialVoiceNumbers = 
  foldr S.insert S.empty . mapMaybe maybeGetVoice . concat . M.elems
  where
    -- Here in maybeGetVoice we enforce all essential voices are "Just v"
    maybeGetVoice :: XMsrData -> Maybe Int
    maybeGetVoice d
      -- case exhaustion in the case statement here means some essential
      -- XMsrData didn't have its voice set
      | hasEssentialVoice d = case getVoice d of {Just v -> Just v}
      | otherwise           = Nothing


-- Given that the minimum existing essential voice number is 'minVoice', then
-- check if this is an essential voice, and if so adjust it in such as way
-- that all voices will be normalized at a minimum of 1.
normalizeVoice :: Int -> XMsrData -> XMsrData
normalizeVoice minVoice d
  | hasEssentialVoice d && currentV >= minVoice = 
      setVoice (currentV-minVoice+1) d
  | not (hasEssentialVoice d) = d
    where currentV = case getVoice d of {Just x -> x}

quickShowMsrData (XMDDirection _ _ v s _) = printf "XMDDirection"
quickShowMsrData (XMDNote (XNRest {} ) _) = printf "XNRest"
quickShowMsrData (XMDNote (XNNote {} ) _) = printf "XNNote"

