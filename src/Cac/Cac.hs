z{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveGeneric,
  DeriveAnyClass, TemplateHaskell, MultiParamTypeClasses,
  FunctionalDependencies #-}

module Cac where

import Control.Lens
import Control.Lens.TH


data N = N
  { _nTimes    :: (Double,Double)
  , _nPitch    :: Int
  , _nLoudness :: Double
  , _nInstr    :: String
  }

makeFields ''N

data Comp = Comp
  { _compNotes :: [N]
  }

makeFields ''Comp


data PSeries = PSeries [Int]

data Scale = Scale [Int]
