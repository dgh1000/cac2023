{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies,
    FlexibleContexts, FlexibleInstances #-}


module Cac.Ptmc.Data where


import Control.Lens
import Control.Lens.TH


data Scale = ScaleMaj     Int
           | ScaleMelMin  Int
           | ScaleHarmMin Int


data Phrase = Phrase
  { _phraseBegPit   :: Int
  , _phraseEndPitGoal :: Int
  , _phraseBegTempo   :: Double
  , _phraseEndTempo   :: Double
  , _phrasePitPat     :: [Int]
  , _phraseScale      :: Scale
  , _phraseDurPat     :: [Double]
  }


makeFields ''Phrase


