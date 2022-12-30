{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances #-}

module Cac.Markov.Data where

import Control.Lens.TH
import Data.Map (Map)
import System.Random
import Control.Monad.State
import Util.RandMonad


type SourceId = ((String, String), Int) -- network set name, network
                                        -- name, and source number

data Note = Note
  { _noteNodeIdens :: [SourceId]
  , _noteTBeg      :: Double
  , _noteTEnd      :: Double
  , _notePitch     :: Int
  , _noteAmpl      :: Double
  , _noteTimbre    :: String
  }
          deriving(Show)


makeFields ''Note

data NoteData = NoteData
  { _noteDataIdens   :: [SourceId]  
  , _noteDataTSpan   :: Maybe Double
  , _noteDataDur     :: Maybe Double
  , _noteDataPitch   :: Maybe Int
  , _noteDataAmpl    :: Maybe Double
  , _noteDataTimbre  :: Maybe String
  }
  

makeFields ''NoteData


emptyNodeData = NoteData [] Nothing Nothing Nothing Nothing Nothing


data Follow es = Follow
  { _followIden    :: Int
  , _followExtraSt :: es
  }


makeFields ''Follow


data Node es = Node
  { _nodeIden       :: Int
  , _nodePitch      :: Maybe Int
  , _nodeTSpan      :: Maybe Double
  , _nodeDur        :: Maybe Double
  , _nodeAmpl       :: Maybe Double
  , _nodeTimbre     :: Maybe String
  , _nodeFollowList :: [Follow es]
  }


makeFields ''Node


{-
data PtrState = PtrState
  { _ptrStateInt   :: Int
  , _ptrStateNextT :: Double
  }
-}


data Network es = Network
  { _networkName      :: String
  , _networkNodes     :: Map Int (Node es)
  , _networkPtrStates :: Int
  }


makeFields ''Network


type M es = State (MState es)


runM :: M es b -> MState es -> (b,MState es)
runM = runState


data MState es = MState
  { msGen            :: StdGen
  , msNetworkSetList :: [NetworkSet es] 
  , msNotes          :: [Note]
  }


type FollowFunc es = Network es -> Node es -> M es Int


data NetworkSet es = NetworkSet
  { _networkSetNextT          :: Double
  , _networkSetNetworks       :: [Network es]
  , _networkSetFollowFunc     :: FollowFunc es
  }


makeFields ''NetworkSet


instance RandMonad (M es) where
  putGen g = modify (\st -> st { msGen = g })
  getGen = gets msGen



