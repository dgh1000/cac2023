{-# LANGUAGE FlexibleInstances #-}

module Cac.SimplerB.ShowSimplerB where


import qualified Data.Map as M
import Cac.SimplerB.SimplerBData
import Util.Showable
import Cac.SimplerB.Percentile
import Cac.SimplerB.Comp01
import Text.Printf
import Control.Lens

showReport :: StepReport c step -> ShowItem
showReport sr = error "foo"
  where

{-
showPercentileOut :: PercentileOut a -> String
showPercentileOut (PercentileOut _ items) =
  unlines $ map (show . fst) items
-}


{-

data ShowInt = ShowInt Int

instance ShowItemClass ShowInt where
  showI (ShowInt i) = SingleLine $ "Just " ++ show i


instance ShowItemClass ShowDouble

showMaybe :: ShowItemClass d => Maybe d -> ShowItem
showMaybe Nothing = SingleLine "Nothing"
showMaybe (Just x) = showI x
-}
