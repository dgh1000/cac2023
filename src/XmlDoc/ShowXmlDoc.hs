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
import Data.List(mapAccumL)



instance ShowItemClass XScore where
  
  showI doc =
    Component "XScore" False [ {- partInfoShowData,-} partsShowData ]

    where
      
      partInfoShowData =
        Component "part infos:" True 
          (map showStringPair . M.toList $ xPartInfos doc)
        
      partsShowData =
        Component "Parts:" True
          (map (showI . NamedXPart) . M.toList . xParts $ doc)

      showStringPair (s1, XPartInfo s2) =
        SingleLine $ printf "part name: %s, part info: %s" s1 s2


data NamedXPart = NamedXPart (String,XPart)


instance ShowItemClass NamedXPart where
  showI (NamedXPart (name,xPart)) = 
    Component (printf "Part name: %s" name) True 
      (map showI $ xpMsrs xPart)
  
instance ShowItemClass XMsr where
  showI (XMsr num _ datas) = 
    Component (printf "measure num: %d" num) True
      (map showI txmd)
    where
      txmd :: [TimedXMsrData]
      txmd = filter noRests $ catMaybes $ snd $ toTimedMsrData datas
      noRests :: TimedXMsrData -> Bool
      noRests (TimedXMsrData _ (XMDNote (XNRest {}))) = False
      noRests x = True

data TimedXMsrData = TimedXMsrData Int XMsrData

toTimedMsrData :: [XMsrData] -> (Int,[Maybe TimedXMsrData])
toTimedMsrData xmsrs = mapAccumL step 0 xmsrs
  where
    step :: Int -> XMsrData -> (Int,Maybe TimedXMsrData) 
    step t d@(XMDDirection _ _ _ _) = (t, Just $ TimedXMsrData t d)
    step t d@(XMDNote n) = (t + xnDuration n, Just $ TimedXMsrData t d)
    step t d@(XMDBackup b) = (t - b, Nothing)
    step t d@(XMDForward f) = (t + f, Nothing)
    step t d@(XMDOther _) = (t, Just $ TimedXMsrData t d)

instance ShowItemClass TimedXMsrData where
  showI (TimedXMsrData t (XMDDirection xdir mOffset mVoice mStaff)) =
    Component ("[" ++ show t ++ "] " ++ (show xdir))
      True  [ SingleLine $ "offset: " ++ show mOffset
            , SingleLine $ "voice: " ++ show mVoice
            , SingleLine $ "staff: " ++ show mStaff ]
  showI (TimedXMsrData t (XMDNote xNote)) = 
    Component (printf "[%d] Pitch: %s" t (showXPitch $ xnPitch xNote)) 
      True [dur,isGrace,isChord,voiceStaff,ties,notations]
        where
          dur = SingleLine $ printf "duration   : %d" (xnDuration xNote)
          isGrace = SingleLine $ printf "isGrace    : %s" (show $ xnIsGrace xNote)
          isChord = SingleLine $ printf "isChord    : %s" (show $ xnChord xNote)
          voi = case xnVoice xNote of 
            Just v -> show v
            Nothing -> "absent"
          sta = case xnStaff xNote of 
            Just s -> show s
            Nothing -> "absent"
          voiceStaff = 
            SingleLine $ 
              printf "voice/staff: %s/%s" voi sta
          ts = case (xnTieStart xNote,xnTieStop xNote) of
            (False,False) -> "no tie"
            (False,True)  -> "tie stop"
            (True,False)  -> "tie start"
            (True,True)   -> "tie start/stop"
          ties = SingleLine ts
          notations = SingleLine $ 
            "Notations  : " ++ concatMap (\n -> show n ++ " ") 
              (xnNotations xNote)
          

  showI (TimedXMsrData t (XMDOther s)) = SingleLine $ printf "[%d] XMDOther: %s" t s

showXPitch :: XPitch -> String
showXPitch (XPitch step alter octave) = printf "%s%s%d" step sAlter octave
  where
    sAlter = case alter of
      -1 -> "b"
      0  -> ""
      1  -> "#"