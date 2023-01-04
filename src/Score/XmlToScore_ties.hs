module Score.XmlToScore_ties where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Map(Map)
import Data.Set(Set)
import Data.List.Index
import Common
import XmlDoc.XmlDocData
import Data.Maybe


xMsrDataToTNote :: Map Int IXMsrInfo -> (Loc,XMsrData) -> Maybe TNote
xMsrDataToTNote mix (begLoc,xmsr) = 
  case maybeXNoteNoteType xmsr of 
    Nothing -> Nothing
    Just (XNNote dur grace _ mVoi mStaff xpitch tieStart 
      tieStop nots notehead _) -> 
        let 
          voi = case mVoi of {Just x -> x}
          order = error "foo"
          endLoc = error "Foo"
          pitch = error "foo"
        in
          Just 
            (TNote pitch voi mStaff tieStart tieStop begLoc endLoc 
              order nots notehead grace)




maybeXNoteNoteType :: XMsrData -> Maybe XNote
maybeXNoteNoteType (XMDNote x@XNNote {} _) = Just x
maybeXNoteNoteType _ = Nothing



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

compareTNote :: TNote -> TNote -> Ordering
compareTNote
  (TNote _ _ _ _ _ beg1 _ order1 _ _ grace1)
  (TNote _ _ _ _ _ beg2 _ order2 _ _ grace2) =
    case (grace1,grace2) of
        (Nothing, Just  _) -> LT
        (Just _ , Nothing) -> GT
        _                  ->
          if beg1 /= beg2 
            then compare beg1 beg2
            else compare order1 order2


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
