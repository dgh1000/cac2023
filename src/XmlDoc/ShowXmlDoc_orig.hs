{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module XmlDoc.ShowXmlDoc where


import Text.Printf
import Text.XML.Light
import Data.Maybe
import qualified Data.Map as M
import Data.Map(Map)
import Util.Showable
import XmlDoc.XmlDocData
import Common
import Common.CommonUtil
import Util.Showable


instance ShowItemClass XScore where
  showI doc = Component "XScore" False [sPartInfos,sParts]
    where
      sPartInfos = Component "part infos:" True 
        (map showI . M.toList . xPartInfos $ doc)
      sParts = Component "Parts:" True (map showI . M.toList . xParts $ doc)

type IdXPartInfo = (String,XPartInfo)

instance ShowItemClass IdXPartInfo where
  showI (id_,XPartInfo name) = SingleLine $ printf "XPartInfo: Id:%s Name:%s"
                               id_ name

type IdXPart = (String,XPart)

instance ShowItemClass IdXPart where
  showI (id_,XPart msrs) = Component (printf "Part '%s':" id_) True 
                           (map showI msrs)

instance ShowItemClass XMsr where
  showI (XMsr i attr datas) = Component 
    (printf "Msr number:%d %s" i sAttr) True (map showI datas)
    where
      sAttr = case attr of 
        Nothing -> "No time/divisions attributes."
        Just a -> printf "dpq:%s beats:%s beat-type:%s" (show $ xaDpq a)
          (show $ xaNumer a) (show $ xaDenom a)

-- data XNote = 
--    XNRest
--      { xnDuration :: Int
--      , xnChord :: Bool
--      , xnVoice :: Maybe Int
--      , xnStaff :: Maybe Int
--      , xnNotations :: [XNotation]
--      , xnOrder :: Int }
--  | XNNote
--      { xnDuration :: Int
--      , xnIsGrace :: Maybe Bool -- True means 'slash' (acciaccatura, before the beat)
--      --  , xnGraceOrder :: Int
--      , xnChord :: Bool
--      , xnVoice :: Maybe Int
--      , xnStaff :: Maybe Int
--      , xnPitch :: XPitch
--      , xnTieStart :: Bool
--      , xnTieStop :: Bool
--      , xnNotations :: [XNotation]
--      , xnNotehead :: Maybe XNotehead
--      , xnOrder :: Int }
--   deriving(Show)

instance ShowItemClass XMsrData where
  showI (XMDNote (XNRest dur ch v s nots _) _) = 
    Component (printf "rest: dur:%d chord:%s voice:%s staff:%s" dur (show ch) 
                       (show v) (show s)) True (map showI nots)
  showI (XMDNote (XNNote dur isGra ch v st pit tieStart tieStop nots _ _) _) = 
    Component (printf ("note dur:%d stp:%s alt:%d oct:%d tie:%s %s") 
               dur (xStep pit)
               (xAlter pit) (xOctave pit) (show tieStart) (show tieStop))
               True ([secondLine] ++ (map showI nots))
      where
        secondLine = SingleLine 
                     (printf "   grace:%s chord:%s voice:%s staff:%s" 
                     (show isGra) (show ch) (show v) (show st))
  showI (XMDDirection dir offset mVoice mStaff _) = 
      SingleLine (printf "%s %s %s %s" (show dir) m voice staff)
    where
      m = case offset of 
        Nothing -> ""
        Just off -> printf "offset:%d" off
      voice = case mVoice of
        Nothing -> ""
        Just v -> printf "voice:%d" v
      staff = case mStaff of {Nothing -> ""; Just s -> "staff:" ++ (show s)}
  showI (XMDBackup dur _) = SingleLine (printf "backup: %d" dur)
  showI (XMDForward dur _) = SingleLine (printf "forward: %d" dur)
  showI (XMDOther s _) = SingleLine (printf "other: %s" s)

instance ShowItemClass XNotation where
  showI (XNSlur s l) = SingleLine $ printf "Slur: %s %s" s ml
    where
      ml = case l of 
        Nothing -> ""
        Just i -> printf "slur number:%d" i
  showI (XNArticulations a) = Component "Articulations:" True (map showI a)
  showI (XNOrnaments a) = Component "Ornaments:" True (map showI a)
  showI XNFermata = SingleLine "Fermata"
  showI XNArpeggiate = SingleLine "Arpeggiate"
  showI (XNTechnical ts) = Component "Technical notations:" True (map showI ts)

instance ShowItemClass XTechnical where
  showI XTOpenString = SingleLine "open-string"

instance ShowItemClass XArticulation where
  showI XAStaccato = SingleLine "Staccato"
  showI XAStaccatissimo = SingleLine "Staccatissimo"
  showI XAAccent = SingleLine "Accent"
  showI XAStrongAccent = SingleLine "Strong accent"
  showI XATenuto = SingleLine "Tenuto"

instance ShowItemClass XOrnament where
  showI o = SingleLine (show o)

type XMsrDataAtLoc = (Loc,[XMsrData])
instance ShowItemClass XMsrDataAtLoc where
  showI (l,d) = Component (showLoc2 l) True (map showI d)

----------------------------------------------------------------------
----------------------------------------------------------------------
--                Text.XML.Light

data ShowSelectiveElement = ShowSelectiveElement Element

data ShowSimpleElement = ShowSimpleElement Element

instance ShowItemClass Content where
  showI (Elem e) = Component "Content cons Elem" True [showI e]
  showI (Text t) = Component "Content cons Text" True [showI t]
  showI (CRef s) = SingleLine $ "Content cons CRef: " ++ s

instance ShowItemClass Element where
  showI e = Component "Element:" True [name,attribs,contents]
    where
      name = SingleLine $ "elname:" ++ (qName . elName $ e)
      attribs = Component "Attr:" True (map showI . elAttribs $ e)
      contents = Component "Content:" True (map showI . elContent $ e)

instance ShowItemClass ShowSimpleElement where
  showI (ShowSimpleElement e) = SingleLine . elementToLine $ e

elementToLine :: Element -> String
elementToLine e = printf "%s %s" (qName . elName $ e) concatAttrs
  where
    f :: Attr -> Maybe String
    f a = case (qName.attrKey$a,attrVal a) of
      ("default-x",_) -> Nothing
      ("default-y",_) -> Nothing
      ("font-family",_) -> Nothing
      ("font-size",_) -> Nothing
      ("font-weight",_) -> Nothing
      ("font-style",_) -> Nothing
      ("color",_) -> Nothing
      ("justify",_) -> Nothing
      ("valign",_) -> Nothing
      ("print-object",_) -> Nothing
      (n,v) -> Just $ printf " [%s:%s]" n v
    concatAttrs = concat . mapMaybe f . elAttribs $ e

instance ShowItemClass ShowSelectiveElement where
  showI (ShowSelectiveElement e) = case qName . elName $ e of
    "score-partwise" -> c2
    "part" -> c2
    "part-list" -> c2
    "attributes" -> c2
    "part-group" -> c2
    "score-part" -> c2
    "measure" -> c2
    "note" -> c2
    "direction" -> c2
    "direction-type" -> c2
    "voice" -> t
    "staff" -> t
    "words" -> t
    "clef" -> c2
    "divisions" -> t
    "key" -> c2
    "time" -> c2
    "staves" -> t
    "dynamics" -> c2
    "metronome" -> c2
    "beat-unit" -> t
    "per-minute" -> t
    "other-direction" -> t
    "part-name" -> t
    _ -> SingleLine . elementToLine $ e
    where
      c2 = Component (elementToLine e) True 
        (map (showI . ShowSelectiveElement) (filterChildren (const True) e))
      t = SingleLine $ printf "%s '%s'" (elementToLine e) (strContent e)
      tNoText = SingleLine (elementToLine e)
      


instance ShowItemClass Attr where
  showI a = SingleLine $ printf "name:%s value:%s" (qName . attrKey $ a)
            (attrVal a)

instance ShowItemClass CData where
  showI c = SingleLine $ printf "cdData:%s" (take 50 . cdData $ c)


----------------------------------------------------------------------
----------------------------------------------------------------------
--              IXmlDoc

{-
instance ShowItemClass IXmlDoc where
  showI (IXmlDoc mis parts) = Component "IXmlDoc" False [sMis,sParts]
    where
      sMis = Component "IXMsrInfo" True (map showI . M.toList $ mis)
      sParts = Component "IXParts" True (map showI . M.toList $ parts)

type PairIXMsrInfo = (Int,IXMsrInfo)
instance ShowItemClass PairIXMsrInfo where
  showI (idx,IXMsrInfo dpq numer denom) 
     = SingleLine $ printf "%3d: dpq:%4d numer:%2d denom:%2d" 
       idx dpq numer denom


type PairIXPart = (String,IXPart)
instance ShowItemClass PairIXPart where
  showI (name, IXPart msrDatas notes) = Component name True [sMsrDatas,sNotes]
    where 
      sMsrDatas = Component "XMsrData" True (map showI (M.toList msrDatas))
      sNotes = Component "XNotes" True (map showI (M.toList notes))
-}



{-
type XNoteAtLoc = (Loc,[XNote])
instance ShowItemClass XNoteAtLoc where
  showI (l,ns) = Component (simpleShowLoc l) True (map showI ns)

instance ShowItemClass XNote where
  -- exhausted cases here, because there is no definition for showI on the
  -- XNRest constructor, means we expected to show XNotes only inside 
  -- IXScores
  showI (XNNote _ endLoc isGrace _ voice staff pitch tieStart tieStop
        notations notehead) 
    = Component (printf "XNNote endLoc:%s voice:%s staff:%s"
      (simpleShowLoc endLoc) (show voice) (show staff)) True [l1,l2,ns]
    where
      showMaybeNotehead Nothing = "Nothing"
      showMaybeNotehead (Just n) = xnhType n 
      l1 = SingleLine $ printf "isGrace:%s step:%s alter:%d octave:%d"
           (show isGrace) (xStep pitch) (xAlter pitch) (xOctave pitch)
      l2 = SingleLine $ printf "tieStart:%s tieStop:%s notehead:%s" 
           (show tieStart) (show tieStop) (showMaybeNotehead notehead)
      ns = Component "Notations:" True (map showI notations)
-}

----------------------------------------------------------------------
----------------------------------------------------------------------
--               testing

type TestPartMap = Map String TestPart
instance ShowItemClass TestPartMap where
  showI m = Component "All parts" False (map showI . M.toAscList $ m)

type PairTestPart = (String,TestPart)
instance ShowItemClass PairTestPart where
  showI (pid,TestPart msrDatas) = Component pid True 
    (map showI . M.toAscList $ msrDatas)
