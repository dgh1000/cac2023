{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances #-}

module Cac.Markov.Data where

import Data.Map(Map)
import Control.Lens.TH
import Control.Monad.State
import System.Random

type NodeId = Int


data Note = Note
  { _noteTBeg    :: Double
  , _noteTEnd    :: Double
  , _notePitch   :: Int
  , _noteAmpl    :: Double
  , _noteTimbre  :: String
  }


data NodePitch = NodePitch
  { _nodePitchI      :: NodeId
  , _nodePitchPitch  :: Int
  }
  | NodePitchOffset
  { _nodePitchI          :: NodeId
  , _nodePitchOffset :: Int
  }


makeFields ''NodePitch


data NodeSpan = NodeSpan
  { _nodeSpanI      :: NodeId
  , _nodeSpanSpan   :: Double
  }
  | NodeSpanOffset
  { _nodeSpanI      :: NodeId
  , _nodeSpanOffset :: Double  -- a ratio
  }


makeFields ''NodeSpan


data NodeDur = NodeDur
  { _nodeDurI      :: NodeId
  , _nodeDurDur    :: Double
  , _nodeDurOffset :: Double -- a ratio
  }
  | NodeDurOffset
  { _nodeDurI      :: NodeId
  , _nodeDurOffset :: Double  -- a ratio
  }


makeFields ''NodeDur


data NodeAmpl = NodeAmpl
  { _nodeAmplI      :: NodeId
  , _nodeAmplDur    :: Double
  , _nodeAmplOffset :: Double -- a difference
  }
  | NodeAmplOffset
  { _nodeAmplI      :: NodeId
  , _nodeAmplOffset :: Double  -- a ratio
  }


makeFields ''NodeAmpl


data Link = Link
  { _linkDest    :: NodeId
  , _linkWeight  :: Double
  }


data Node = Node
  { _nodeMPitch :: Maybe NodePitch
  , _nodeMSpan  :: Maybe NodeSpan
  , _nodeMDur   :: Maybe NodeDur
  , _nodeMAmpl  :: Maybe NodeAmpl
  , _nodeFollow :: [Link]
  }


makeFields ''Node


type Network = Map NodeId Node


-- <current node id>  <time it was arrived at>
--
data PointerState = PointerState
  { _traversalStateI       :: NodeId
  , _traversalStateT0      :: Double
  , _traversalStateMPitch  :: Maybe Int
  , _traversalStateMSpan   :: Maybe Double
  , _traversalStateMDur    :: Maybe Double
  , _traversalStateMAmpl   :: Maybe Double
  }


data MState = MState
  { _mStateGen           :: StdGen
  , _mStateNetwork       :: Network
  , _mStatePointerStates :: [PointerState]
  }
  

type M = State MState


