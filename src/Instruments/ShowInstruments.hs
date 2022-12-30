{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Instruments.ShowInstruments where

import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Map.Strict(Map)
import Text.Printf
import Util.Showable
import Common
import Common.CommonUtil
import Score.ScoreData
import Instruments
import Translation
import Translation.TimeMap






{-
instance ShowItemClass OnOff where
  showI (OnOff x) = ooHelp x
-}


{-
type StaffName_TimeMods = (String,[UnitTimeMod])
instance ShowItemClass StaffName_TimeMods where
  showI (staffN,mods) = Component staffN True (map showI mods)
-}
{-

ooHelp [] = SingleLine "empty OnOff"
ooHelp [(msg,(on,off))] = SingleLine $ printf "%s: %8.3f %8.3f" msg on off
ooHelp mult = Component "on/off" True (map doOne mult)
  where
    doOne (msg,(on,off)) = SingleLine $ printf "%20s--%8.3f %8.3f" msg on off
-}


----------------------------------------------------------------------
--           mainly debugging output


