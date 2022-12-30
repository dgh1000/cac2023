
module Uvi.Show where

import qualified Data.Map as M
import Text.Printf
import Score.ScoreData
import Util.Showable
import Uvi
import Common
import Common.CommonUtil


instance ShowItemClass PatchSection where
  showI (PatchSection name _ marks beg end keys) =
    Component (printf "PatchSection: %s, %s->%s" name (showLoc2 beg) (showLoc2 end)) True
      [sMarks,sKeys]
        where sMarks = Component "Marks:" True (map f $ M.toAscList marks)
              f :: (Loc,[Mark Double]) -> ShowItem
              f (loc,ms) = Component (showLoc2 loc) True (map (SingleLine . show) ms)
              sKeys = Component "ChordDatas" True (map showI keys)

instance ShowItemClass ChordData where
  showI (ChordData _ loc vn ch notes) = SingleLine $ printf "loc:%s vn:%d"
    (showLoc2 loc) vn
              
        
