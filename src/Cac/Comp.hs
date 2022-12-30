{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies,
             FlexibleInstances #-}


module Cac.Comp where

import qualified Data.Map as M
import Data.Map(Map)
import Util.Math
import Control.Lens
import Control.Lens.TH

data N = N
  { _nTimes    :: (Double,Double)
  , _nPitch    :: Int
  , _nLoudness :: Double
  , _nTimbre   :: String
  }

data Comp = Comp
  { _compNotes :: [N]
  }

makeFields ''Comp

data CompF = CompF
  { _compFCompFg :: [(Int,Int)]
  , _compFCompMg :: [(Int,Int)]
  , _compFCompBg :: [(Int,Int)]
  }

makeFields ''CompF

data Set3 = Set3 [(Int,Int)]







 
{-
data MNote = MNote
  { mnT :: Int
  , mnTEnd :: Int
  , mnChan :: Int
  , mnPitch :: Int
  , mnVel   :: Int
  }




-- 1: ppp, 2:pp, 3:p, 4: mp 
loudToVel :: Double -> Int
loudToVel l = round $ scaleClip 1 l 8 10 127


compToMidi :: Comp -> [MNote]
compToMidi (Comp cns) = out
  where
    out = concatMap g $ M.toAscList cns
    g (_,cs) = map h cs
    h (CNote t tEnd p loud) = MNote t tEnd 1 p (loudToVel loud) 
    
-}

