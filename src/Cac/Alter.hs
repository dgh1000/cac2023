
module Cac.Alter where


data N = N
  { _nTimes :: [(Double,Double)]
  , _nPitch :: Int
  , _nDyn   :: Double
  , _nInstr :: String
  }


data Comp = Comp
  { _compNotes :: [N]
  }

slice :: Double -> Double -> [N]
slice 
