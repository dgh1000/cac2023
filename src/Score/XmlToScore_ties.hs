module Score.XmlToScore_ties where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Map(Map)
import Data.Set(Set)
import Common ( Pitch(Pitch, midiPitch), Loc(Loc) )
import XmlDoc.XmlDocData
import Data.Maybe
import Debug.Trace

-- xMsrDataToTNote :: Map Int IXMsrInfo -> [(Loc,XMsrData)]

tNotesToVoicesLocs :: [TNote] -> Map Loc (Map Int [TNote])
tNotesToVoicesLocs tns = f5
  where
    -- tnVoice
    -- tnBegin
    f1 :: TNote -> (Loc,[TNote])
    f1 n = (tnBegin n, [n])
    f2 :: Map Loc [TNote]
    f2 = M.fromListWith (++) . map f1 $ tns
    f3 :: TNote -> (Int,[TNote])
    f3 n = (tnVoice n, [n])
    f4 :: [TNote] -> Map Int [TNote]
    f4 = M.fromListWith (++) . map f3
    f5 = M.map f4 f2


doTiesXMsrData :: Map Int IXMsrInfo -> [(Loc,XMsrData)] ->
  [TNote]
-- doTiesXMsrData imix xs = doTies ts
doTiesXMsrData imix xs = "not doing ties! " `trace` ts
  where
    ts :: [TNote]
    ts = mapMaybe (xMsrDataToTNote imix) xs


xMsrDataToTNote :: Map Int IXMsrInfo -> (Loc,XMsrData) -> Maybe TNote
xMsrDataToTNote mix (begLoc,xmsr) = 
  case maybeXNoteNoteType xmsr of 
    Nothing -> Nothing
    Just (XNNote dur grace _ mVoi mStaff xpitch tieStart 
      tieStop nots notehead _) -> 
        let 
          voi = case mVoi of {Just x -> x;Nothing->error"foo"}
          order = orderOf xmsr
          -- computeEndLoc :: Map Int IXMsrInfo -> Loc -> Int -> Loc
          endLoc = computeEndLoc mix begLoc dur
          pitch = xPitchToPitch xpitch
        in
          Just 
            (TNote pitch voi mStaff tieStart tieStop begLoc endLoc 
              endLoc order nots notehead grace)
    _ -> error "Foo"


maybeXNoteNoteType :: XMsrData -> Maybe XNote
maybeXNoteNoteType (XMDNote x@XNNote {} _) = Just x
maybeXNoteNoteType _ = Nothing


xPitchToPitch (XPitch stepString alter octave) =
  Pitch midiPitch step alter octave
  where
    step = case lookup stepString [ ("C",0),("D",1),("E",2),("F",3)
                                  , ("G",4),("A",5),("B",6)] of 
      Just x -> x
    pitchClass = 
      case lookup stepString [("C",0),("D",2),("E",4),("F",5),("G",7)
                             ,("A",9),("B",11)] of
        Just x -> x
    midiPitch = (octave+1) * 12 + pitchClass + alter


computeEndLoc :: Map Int IXMsrInfo -> Loc -> Int -> Loc
computeEndLoc xmis (Loc locMsr locBeat) dur = 
  case M.lookup locMsr xmis of
    Just (IXMsrInfo dpq numer denom)
      -- exhausted case means number of divs put end loc past beat 1 of next msr
      | e == fromIntegral numer + 1 -> Loc (locMsr+1) 1
      | e <  fromIntegral numer + 1 -> Loc locMsr e
      where
        e = locBeat + (fromIntegral denom / 4) *
            (fromIntegral dur / fromIntegral dpq)

--------------------------------------------------------------------
--------------------------------------------------------------------
---------------------------- doTies --------------------------------

-- If we sort TNotes by grace note first, Loc second, 
-- and order within measure third, then we are set up to process one
-- TNote at a time in backwards order
-- 

doTies :: [TNote] -> [TNote]
doTies tns = catMaybes mTns
  where
    (_,mTns) = L.mapAccumR step1 M.empty (L.sortBy compareTNote tns)

-- compare Loc, then grace, then order
compareTNote :: TNote -> TNote -> Ordering
compareTNote
  (TNote _ _ _ _ _ beg1 _ _ order1 _ _ grace1)
  (TNote _ _ _ _ _ beg2 _ _ order2 _ _ grace2) =
    case compare beg1 beg2 of
      LT -> LT
      GT -> GT
      _  -> case (grace1,grace2) of
        (Nothing, Just  _) -> GT
        (Just _ , Nothing) -> LT
        _                  -> compare order1 order2


step1 :: Map Loc [TieEnd] -> TNote -> (Map Loc [TieEnd], Maybe TNote)
step1 tes tn = out
  where
    -- if this is a tie end but not a tie start
    out = case (tnTieStart tn,tnTieStop tn) of
      (False,False) -> (tes, Just tn)
      (False,True ) -> (M.insertWith (++) (tnBegin tn) [newTieEnd] tes, Nothing)
      (True ,_    ) -> stepHandleTie tes tn
      where
        newTieEnd = TieEnd (tnPitch tn) (tnVoice tn) (tnStaff tn) (tnEnd tn)



stepHandleTie :: Map Loc [TieEnd] -> TNote -> (Map Loc [TieEnd], Maybe TNote)
stepHandleTie tes tn = out
  where
    out = case M.lookup (tnEnd tn) tes of
      Nothing -> (tes,Just tn)
      Just teList ->
        let p = tnPitch tn
            matchingTes = filterMatchPitch p teList
            matchingTesVoice = filterMatchVoice (tnVoice tn) matchingTes
            matchingTesStaff = filterMatchStaff (tnStaff tn) matchingTes
            maybeTieEnd1 te | tnTieStop tn = Nothing
                            | otherwise    = Just $ tn { tnEnd = teEnd te }
            maybeTieEnd2 | tnTieStop tn = Nothing
                         | otherwise    = Just tn
        in
            case (matchingTesVoice,matchingTesStaff) of
              (x:_, _) -> (updateTesMap tn x tes,maybeTieEnd1 x)
              ([],x:_) -> (updateTesMap tn x tes,maybeTieEnd1 x)
              _        -> (tes                  ,maybeTieEnd2)

updateTesMap :: TNote -> TieEnd -> Map Loc [TieEnd] -> Map Loc [TieEnd]
updateTesMap tn te = M.insertWith (++) (tnBegin tn) [te]


filterTieStart :: [TNote] -> [TNote]
filterTieStart = filter tnTieStart


filterMatchPitch :: Pitch -> [TieEnd] -> [TieEnd]
filterMatchPitch (Pitch midi _ _ _) =
  filter (\e -> midi == (midiPitch . tePitch $ e))


filterMatchVoice :: Int -> [TieEnd] -> [TieEnd]
filterMatchVoice vn = filter (\e -> vn == teVoice e)


filterMatchStaff :: Maybe Int -> [TieEnd] -> [TieEnd]
filterMatchStaff mStaff = filter (\e -> mStaff == teStaff e)
