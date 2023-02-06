{-#  LANGUAGE TypeSynonymInstances, FlexibleInstances, ScopedTypeVariables #-}
module Score.ShowScore where

import qualified Common as CD
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map(Map)
import Text.Printf
import Score.ScoreData
import Common
import Common.CommonUtil(simpleShowLoc,showLoc2)
import Util.Showable
import XmlDoc.ShowXmlDoc

----------------------------------------------------------------------
----------------------------------------------------------------------
--                  ScoreObjects



----------------------------------------------------------------------
----------------------------------------------------------------------
--                      Instances of Showable

------------- Showable for Score

data ShowLoc = ShowLoc Loc

instance ShowStringClass ShowLoc where
  showS (ShowLoc l) = showLoc2 l

----------------------------------------------------------------------
--   instance of ShowItemClass for [MarkD]

data MarkDList = MarkDList [MarkD]

instance ShowItemClass MarkDList where
  showI (MarkDList ms) = SingleLine "MarkD: to be implemented"
  

----------------------------------------------------------------------
--          instance of ShowItemClass for Staff

instance ShowItemClass Staff where
  showI = error "instance TBD"


----------------------------------------------------------------------
--          instance of ShowItemClass for Score

instance ShowItemClass Score where
  showI (Score timeSigs marks _ _ staves _) = 
    Component "Score" False
      [ 
        -- Component "Time signatures" True 
        -- (map (\(num,TimeSig numer denom) -> SingleLine $ printf "%3d: %2d/%2d"
        --       num numer denom) . M.toAscList $ timeSigs)
        shownMarks
      , showI staves2
      ]
      
    where

      marks2 :: ShowMap Loc (ShowMap String MarkDList)
      marks2 = myTransformMap marks MarkDList
      shownMarks = showI marks2
      
      staves2 :: ShowMap String Staff
      staves2 = ShowMap staves



myTransformMap :: forall a b c d.
  Map a (Map b c) -> (c -> d) -> ShowMap a (ShowMap b d)
myTransformMap m f = ShowMap d1
  where
    m1 :: Map b c -> Map b d
    m1 = M.map f

    m2 :: Map b d -> ShowMap b d
    m2 = ShowMap

    m3 :: Map a (Map b c) -> Map a (ShowMap b d)
    m3 = M.map (m2 . m1)

    d1 :: Map a (ShowMap b d)
    d1 = m3 m

type VoicePrelimChord = (Int,PrelimChord)
instance ShowItemClass VoicePrelimChord where
  showI (vn,PrelimChord end mods notes grType graces) =
    Component (printf "PrelimChord vn:%d end:%s" vn (simpleShowLoc end)) 
      True [sNotes,sGrace,sMods]
      where
        sNotes = Component "Notes" True (map showI notes)
        sGrace = case graces of
          [] -> SingleLine "No grace notes."
          gs -> Component (printf "Grace type: %s" (show grType)) True
            (map showI graces)
        sMods = SingleLine $ show mods

type LocVoicePrelimChord = (Loc,Map Int PrelimChord)
instance ShowItemClass LocVoicePrelimChord where
  showI (loc,m) = Component (printf "beg: %s" (simpleShowLoc loc)) True
    (map showI $ M.toAscList m)

type MapLocVoicePrelimChord = Map Loc (Map Int PrelimChord)
instance ShowItemClass MapLocVoicePrelimChord where
  showI m = Component "staff" True (map showI $ M.toAscList m)

type ListMapLocVoicePrelimChord = [(String,Map Loc (Map Int PrelimChord))]
instance ShowItemClass ListMapLocVoicePrelimChord where
  showI ms = Component "doc" False (map g ms)
    where
      g (s,m) = Component s True xs
        where
          Component _ _ xs = showI m


{-

type StringMergedNumTechnique = (String,Map Loc [Int])
instance Showable StringMergedNumTechnique where
  showI (s,m) = Component (printf "'%s'" s) True (map showI . M.toAscList $ m)

type LocMergedNumTechnique = (Loc,[Int])
instance Showable LocMergedNumTechnique where
  showI (loc,values) = SingleLine $ (simpleShowLoc loc) ++ " " ++ show values

-}

{-

       stuff removed April 2019


----------------------------------------------------------------------
----------------------------------------------------------------------
--                        Staff




instance ShowListClass Staff where
  showL (Staff _ dyns hairp ped metMarks maxEnd _ slurs chords) =
    [ Component "Dynamics"  True [showI $  WrappedMap dyns]
    , showI ("Hairpins",     map (SingleLineTup 20 20) $ M.toAscList hairp)
    , showI ("Pedal events", map (SingleLineTup 20 20) $ M.toAscList ped)
    , SingleLine $ printf "max true end: %s" (showLoc2 maxEnd)
    -- , Component "True ends"    True (map showI . M.toAscList $ trEnds)
    -- , Component "Time map"     True (map showI . M.toAscList $ timeMap)
    , Component "slurs" True $ map showI $ M.toAscList slurs
    -- , Component "brackets" True $ map showBr $ M.toAscList brackets
    ] ++ showL (WrappedMap chords) 
    where
      showBr (name,locPairs) = Component name True (map g locPairs)
        where
          g (loc1,loc2) = SingleLine $ showLoc2 loc1 ++ " " ++ showLoc2 loc2

----------------------------------------------------------------------
----------------------------------------------------------------------
--                      PedalEvt

instance ShowStringClass PedalEvt where
  showS = show

----------------------------------------------------------------------
----------------------------------------------------------------------

type LocLoc = (Loc,Loc)

instance ShowItemClass LocLoc where
  showI (loc1,loc2) =
     SingleLine $ printf "%s %s" (showLoc2 loc1) (showLoc2 loc2)

----------------------------------------------------------------------
----------------------------------------------------------------------
--                      showing Marks

instance ShowItemClass MarkD where
  showI (TrillShapeMark (TrillShape step1 segs step2)) =
    SingleLine $ printf "Trill shape: %s %s; %s" (show step1) (show step2)
      ((concatMap (\(f1,d2) -> printf "%8.3f %d, " f1 d2) segs) :: String)
  showI x = SingleLine $ show x 
  


type LocOctLines = (Loc,OctaveLine)
instance ShowItemClass LocOctLines where
  showI (loc,OctaveLine n end) = SingleLine $ printf "%s n:%3d end:%s" 
         (simpleShowLoc loc) n (simpleShowLoc end)

-}

instance ShowListClass (Map Int Chord) where
  showL = map g . M.toAscList
    where
      g :: (Int,Chord) -> ShowItem
      g (vn,Chord endLoc mods notes graces) = 
          Component (printf "Voi %d chord: end:%s %s"
                     vn (showLoc2 endLoc)
                     (concatMap (\m -> " " ++ show m) $ S.toList mods))
          True [sNotes,sGraces]
        where
          sNotes = case notes of
            NSingles m -> Component "NSingles" True (map showI $ M.toAscList m)
            NTrill fl m1 m2 -> Component ("NTrill " ++ show fl) True
               [ Component "m1" True (map showI $ M.toAscList m1)
               , Component "m2" True (map showI $ M.toAscList m2)
               ]
          sGraces = case graces of 
            [] -> SingleLine "No grace notes."
            xs -> Component "Grace notes: " True (map showI graces)
          -- prepend s1 (SingleLine s2) = SingleLine $ s1 ++ s2



type IndexNote = (Int,Note) 
instance ShowItemClass IndexNote where
  showI (idx,Note (Pitch midi step alter _) isTied trueEnd _) =
    SingleLine $ printf ("%d: midi:%2d step:%d alter:%2d tieStart:%3s " ++ 
      "trueEnd:%s") idx midi step alter (yesno isTied)
      (simpleShowLoc trueEnd)
    where yesno True = "Yes"
          yesno False = "No"

{-
type TimeMapSlice = (Loc,(Rational,Double))
instance ShowItemClass TimeMapSlice where
  showI (loc,(sliceDur,t)) = SingleLine $ printf "%s %10s %9.3f" 
                             (simpleShowLoc loc) (show sliceDur) t

type LocSymbols = (Loc,[Symbol])
instance ShowItemClass LocSymbols where
  showI (loc,syms) = Component (simpleShowLoc loc) True (map showI syms)
  
instance ShowItemClass Symbol where showI = SingleLine . show

type LocDyns = (Loc,[Dynamic])
instance ShowItemClass LocDyns where
  showI (loc,ds) = Component (simpleShowLoc loc) True (map showI ds)

instance ShowItemClass Dynamic where
  showI (SimpleDyn level vn) = SingleLine $ printf "SimpleDyn %2d voice:%d"
    level vn
  showI (Fp vn) = SingleLine $ printf "Fp voice:%d" vn

type LocHairpin = (Loc,Hairpin)
instance ShowItemClass LocHairpin where


  showI (loc,Hairpin type_ end) = SingleLine $ printf "%s %11s end:%s"
    (simpleShowLoc loc) (show type_) (simpleShowLoc end)


instance ShowStringClass Hairpin where
  showS (Hairpin type_ end) = printf "%11s end:%s"
                                (show type_) (showLoc2 end)


type LocTexts = (Loc,[Text])
instance ShowItemClass LocTexts where
  showI (loc,texts) = Component (simpleShowLoc loc) True (map showI texts)

instance ShowItemClass Text where
  showI (TechniqueText s)  = SingleLine $ printf "Technique:  '%s'" s
  showI (ExpressionText s) = SingleLine $ printf "Expression: '%s'" s

type LocPedalEvt = (Loc,PedalEvt)
instance ShowItemClass LocPedalEvt where
  showI (loc,p) = SingleLine $ printf "%s %12s" (simpleShowLoc loc) (show p)


-}

{-

type LocNoteKeys = (Loc,[NoteKey])
instance ShowItemClass LocNoteKeys where
  showI (loc,keys) = Component (simpleShowLoc loc) True
    (map (SingleLine . briefShowNoteKey) keys)
-}


{-

briefShowNoteKey :: NoteKey -> String
briefShowNoteKey (NoteKey (ChordKey _ begLoc vn _) n) =
  printf "at %s: voi:%d pit:%d" (simpleShowLoc begLoc) vn 
  (CD.midiPitch . nPitch $ n)
-}
