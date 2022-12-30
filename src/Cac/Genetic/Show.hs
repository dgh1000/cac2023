module Cac.Genetic.Show where

import Text.Printf
import Util.Showable
import Cac.Genetic.Data
import Data.Ratio


instance MotiveClass a => ShowItemClass (MotiveElem a) where
  showI (MeNote p dur span) = SingleLine $
    printf "MeNote %.3f %s" (fromRational dur :: Double) (showP p)
  showI (MeRest span) = SingleLine $ printf "%.3f" (fromRational span::Double)


instance MotiveClass a => ShowItemClass (MotiveElemList a) where
  showI (MotiveElemList ls) = Component "MotiveElemList" True (map showI ls)
