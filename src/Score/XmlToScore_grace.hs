{-# LANGUAGE TupleSections, ScopedTypeVariables #-}
module Score.XmlToScore_grace (splitGrace) where

import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Maybe
import Data.Map(Map)
import Data.Set(Set)
-- import Score.ParseMarks(computeWordMarks,combineMacroDefns,matchBrackets)
import XmlDoc.XmlDocExport
import XmlDoc.Process(computeXmlStaves)
import Common
import Util.Exception
import Util.Map
import Common.CommonUtil
import Score.ScoreData

----------------------------- FUNCTIONS -----------------------------------------

-- documentation about definitions needed to create stGrace in Staff
--         --  stGrace        :: Map Loc (Map Int GraceNoteSeq)
--         , stGrace          = M.empty -- toGraceNoteSeq graces
--         }
-- where
--   -- splitGrace :: Map Loc [XMsrData] -> 
--   --           (Map Loc [XMsrData], Map Loc [XMsrData])
--   -- toGraceNote :: XMsrData -> GraceNote
--   -- toGraceNoteSeq :: Map Loc [GraceNote] -> Map Loc GraceNoteSeq
--   -- toGraceNoteSeq = M.map toGraceNoteSeq2
--   (xmlStaff,graces) = splitGrace xmlStaff1

-- splitGrace :: Map Loc [XMsrData] -> 
-- (Map Loc [XMsrData], Map Loc [XMsrData])

-- we're going to keep something like split grace, but we need to create
-- [XMsrData] -> Map Int [XMsrData] : THIS WILL BE splitOutVoices
-- then map over it XMsrData -> GraceNote
-- then [GraceNote] -> GraceNoteSeq: toGraceNoteSeq





-- splitGrace
--   xstaff -> (without grace notes, with grace notes)
splitGrace :: Map Loc [XMsrData] ->
              (Map Loc [XMsrData], Map Loc [XMsrData])
splitGrace mapIn =
  (listToLMap l1,listToLMap l2)
  where
    -- Map Loc [XMsrData] -> Map Loc [(Loc,XMsrData)]
    ml = lMapToList mapIn
    isGrace :: (Loc,XMsrData) -> Bool
    isGrace (_,XMDNote XNNote {xnIsGrace = result} _) = isJust result
    isGrace (_,_                                  ) = False
    l1 = filter (not . isGrace) ml
    l2 = filter isGrace ml

toGraceNote :: XMsrData -> GraceNote
toGraceNote (XMDNote XNNote { xnVoice = vn
                             , xnPitch = xpitch
                             , xnTieStart = tieStart
                             , xnIsGrace = isGrace } _) =
  GraceNote (voiceNumber vn) (xPitchToMidiPitch xpitch) tieStart typ
  where
    voiceNumber (Just i) = i
    typ = case isGrace of Just b -> b


-- Bool: True = acci, False = appo
-- data GraceNoteSeq = GraceNoteSeq Bool [GraceNote]

toGraceNoteSeq :: Map Loc [GraceNote] -> Map Loc GraceNoteSeq
toGraceNoteSeq = M.map toGraceNoteSeq2
  where
    -- toData :: XMsrData -> (Just Bool,Int)
    -- toData (XMDNote (XNNote { xnIsGrace = result 
    --                         , xnPitch = pit     
    --                         })) = (result,pit)
    -- getPitches :: [XMsrData] -> [Int]
    -- getPitches = map (xPitchToMidiPitch . snd . toData)
    -- [GraceNote] -> GraceNoteSeq
    toGraceNoteSeq2 gns = GraceNoteSeq typ gns
      where
        typ = case L.uncons gns of
          Just (a, _) -> gnType a

-- splitOutVoices :: [XMsrData] -> Map Int [XMsrData]
-- splitOutVoices xs 


{-
-- grace3: 
--   - create grace-changed-notices (but don't alter Locs of normal notes)
--   - alter locs of grace notes
--   - return two separate maps
grace3 :: LMapXNote -> (LMapXNote,LMapXNote)
grace3 m = error "foo"
  where
    (graces,nonGraces) = grace1 m
    -- look up for each Loc in Graces and each voice number a list of graces
    --    zip them with number 
    --    map to changed Loc
    --    create grace-changed-notice
    --    create map of new grace notes
    --    merge with non-grace map


-- grace4: 
--     - ALTER LOC OF GRACE NOTES AT ONE LOC
--     - CREATE CHANGE NOTICE FOR ONE LOC
--        
grace4 :: Loc -> [XNote] -> ((Loc,Loc),[(Loc,XNote)])
grace4 origLoc notes = error "foo"


-- (xnote map) -> (orig with grace notes removed, grace notes only)
grace1 :: LMapXNote -> (LMapXNote,LMapXNote)
grace1 m1 = (condenseDoubleLMap a, condenseDoubleLMap b)
  where
    (a, b) = grace2 $ explodeDoubleLMap m1 

grace2 :: ListVnXNote -> (ListVnXNote,ListVnXNote) 
grace2 l = (graces,catMaybes other)
  where
    (graces,other) = grace2_1 l

grace2_1 :: ListVnXNote -> (ListVnXNote,[Maybe (Loc,(Int,XNote))])
grace2_1 = L.mapAccumL grace2_go []

grace2_go :: ListVnXNote -> (Loc,(Int,XNote)) -> (ListVnXNote,Maybe (Loc,(Int,XNote))) 
grace2_go as (loc,(vn,n))
  | xNoteIsGrace n = (((loc,(vn,n)):as), Nothing          )
  | otherwise      = (as               , Just (loc,(vn,n)))
-}

xNoteIsGrace :: XNote -> Bool
xNoteIsGrace XNNote {xnIsGrace = g} = isJust g
xNoteIsGrace _ = False

explodeDoubleLMap :: (Ord k, Ord j) =>  Map k (Map j [a]) -> [(k,(j,a))]
explodeDoubleLMap =  foo3 . foo2 . foo1
  where
    -- foo1 :: (Ord k, Ord j) => Map k (Map j [a]) -> [(k, Map j [a])]
    foo1 = M.toAscList
    -- foo2 :: (Ord k, Ord j) => [(k, Map j [a])] -> [(k, [(j, a)])]
    foo2 = map (\(a, b) -> (a, lMapToList b))
    -- foo3 :: [(k, [(j,a)] )] -> [(k,(j,a))]
    foo3 = concatMap (\(a,bs) -> map (a,) bs)


condenseDoubleLMap :: (Ord k, Ord j) => [(k,(j,a))] -> Map k (Map j [a])
condenseDoubleLMap m = v2
  where
    -- v1 :: Map k [(j,a)]
    v1 = fromListWithPlusPlus m
    -- v2 :: Map k (Map j [a])
    v2 = M.map fromListWithPlusPlus v1


{-
-- we need to know the location of the grace note. how do we find location
-- of Notes? It seems likely we won't be including grace 
computeGrace :: Map Int IXMsrInfo -> Map Loc [XMsrData] -> 
                Map Loc (AcciSeq,AppoSeq)
computeGrace msrInfo = 
  M.map groupAcciAppo
  -- this leads to Map Loc [Grace]
  . lMapMaybe xMsrDataToGraceNote


xMsrDataToGraceNote :: XMsrData -> Maybe Grace
xMsrDataToGraceNote (XMDNote n@XNNote{}) 
  = case xnIsGrace n of 
      Nothing    -> Nothing
      Just True  -> Just $ Acciaccatura $ xPitchToMidiPitch $ xnPitch n
      Just False -> Just $ Appogiatura $ xPitchToMidiPitch $ xnPitch n
xMsrDataToGraceNote _ = Nothing


groupAcciAppo :: [Grace] -> (AcciSeq,AppoSeq)
groupAcciAppo graces = (AcciSeq $ combine l1,AppoSeq $ combine l2)
  where
    (l1,l2) = L.partition isAcci graces
    isAcci (Acciaccatura _) = True
    isAcci _                = False
    getMP (Acciaccatura p) = p
    getMP (Appogiatura  p) = p
    combine :: [Grace] -> [Int]
    combine gs = map getMP gs
-}

xPitchToMidiPitch :: XPitch -> Int
xPitchToMidiPitch (XPitch stepString alter octave) = midiPitch
  where
    step = case lookup stepString [ ("C",0),("D",1),("E",2),("F",3)
                                  , ("G",4),("A",5),("B",6)] of
      Just x -> x
    pitchClass =
      case lookup stepString [("C",0),("D",2),("E",4),("F",5),("G",7)
                             ,("A",9),("B",11)] of
        Just x -> x
    midiPitch = (octave+1) * 12 + pitchClass + alter
