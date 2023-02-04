module Translation.ShowTranslation where

import Text.Printf
import qualified Data.Map as M
import Util.Showable
import Common.CommonUtil
import Common
import Translation
import Score.ScoreData
import Translation.TimeMap

instance ShowItemClass Curve where
  showI (Curve cs) = Component "Curve" True (map showCurve cs)


showCurve :: OneCurve -> ShowItem
showCurve (OneCurve segMap timeSigs tm) =
  Component "OneCurve" True (map (SingleLine . showSeg) $ M.toAscList segMap)
  where
    showSeg (x1,Seg x2 y1 y2) =
      printf "%20s%20s%8.3f%8.3f" (showLoc2 l1) (showLoc2 l2) y1 y2
      where
        l1 = timeToLoc x1 timeSigs tm
        l2 = timeToLoc x2 timeSigs tm

isShort (Chord _ mods _ _) = Staccato `elem` mods

instance ShowItemClass SNote where
    showI (SNote desc hist staffN loc end2 vn ch note onOff loud dest
           pit nomPit vel mods alterEnd sepSame trill _) =
      SingleLine $ printf "t1: %.4f t2: %.4f" t1 t2 
        where
          (t1,t2) = case onOff of {(_,x):_ -> x}
  
instance ShowItemClass DestData where
  showI (DestData (stream, chan) pitch vel mods)
    = Component (printf "(%2d,%2d) pit:%2d vel:%3d" stream chan pitch vel) True
                (map showI mods)


instance ShowItemClass TrRaw where
  showI (TrRaw staffName time (stream,chan) status data1 data2) =
    SingleLine $ printf "TrRaw (%2d,%2d) %2x %3d %3d" stream chan status data1
                        data2

instance ShowItemClass Modif where
  showI (ModifKs timing ks) = SingleLine $ "ModifKs: " ++ show ks
            
  

instance ShowItemClass UnitTimeMod where
  showI (UnitWarp mStaffName (Left (loc1,loc2)) amt) =
    SingleLine $ printf "%s Left (%s,%s)" (show mStaffName) (showLoc2 loc1)
                  (showLoc2 loc2)
  showI (UnitWarp mStaffName (Right (loc1,loc2,loc3)) amt) =
    SingleLine $ printf "%s Right (%s,%s,%s)" (show mStaffName)
                  (showLoc2 loc1) (showLoc2 loc2) (showLoc2 loc3)
  showI (UnitAdjust mStaffName _ _ _ _) =
    SingleLine $ printf "UnitAdjust %s" (show mStaffName)

instance ShowItemClass RelTimeMap where
  showI (RelTimeMap tm) = Component "" True (map g $ M.toAscList tm)
    where
      g :: (Loc,Double) -> ShowItem
      g (loc,t) = SingleLine $ printf "%s: %5.4f" (showLoc2 loc) t


  