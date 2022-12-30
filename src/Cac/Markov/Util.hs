{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies,
    FlexibleInstances, TypeSynonymInstances #-}

module Cac.Markov.Util where

import qualified Data.Map as M
import Text.Printf
import Cac.Markov.Data
import Util.Showable


data ListNote = ListNote [Note]


instance ShowItemClass Note where
  showI (Note _ tBeg tEnd pitch ampl timbre) =
    Component "Note" True [sLine1, sLine2]
      where
        sLine1 = SingleLine $
          printf "tBeg:%8.3f tEnd:%8.3f pitch:%d" tBeg tEnd pitch
        sLine2 = SingleLine $
          printf "ampl:%8.3f timbre:%s" ampl timbre


instance ShowItemClass ListNote where
  showI (ListNote ns) = Component "Notes" True (map showI ns)


instance ShowItemClass (NetworkSet es) where
  showI (NetworkSet nextT networks _) =
    Component "NetworkSet" True [sNextT, sNetworks]
      where
        sNextT    = SingleLine $ printf "nextT: %8.3f" nextT
        sNetworks = Component "networks" True (map showI networks)


data ListNetworkSet es = ListNetworkSet [NetworkSet es]


instance ShowItemClass (ListNetworkSet es) where
  showI (ListNetworkSet ns) = Component "Network Set List" True (map showI ns)


{-
data Network es = Network
  { _networkNodes     :: Map Int (Node es)
  , _networkPtrStates :: Int
  }
-}

instance ShowItemClass (Network es) where
  showI (Network _ nodes ptrSt) = Component "Network" True [sPtrSt, sNodes]
    where
      sPtrSt = SingleLine $ "ptrState: " ++ printf "%i" ptrSt
      sNodes = Component "Nodes" True (map showI $ M.toList nodes)

type IntNode es = (Int,Node es)

instance ShowItemClass (IntNode es) where
  showI (iden1, Node iden pitch tSpan dur ampl timbre followList) =
    Component (printf "iden: %i" iden1) True [sNode]
      where
        showInt :: Int -> String
        showInt i = show i
        showDouble d = printf "%8.3f" d
        sNode = Component "Node" True [line1, line2, line3]
        line1 = SingleLine $
          printf "iden: %d, pitch: %s" iden (showMaybe showInt 11 pitch) 
        line2 = SingleLine $
          printf "tSpan: %s dur: %s" (showMaybe showDouble 11 tSpan)
            (showMaybe showDouble 11 dur)
        line3 = SingleLine $
          printf "ampl: %s, timbre: %s" (showMaybe showDouble 11 ampl)
            (showMaybe id 11 timbre)


instance ShowItemClass (MState es) where
  showI (MState _ netSetList notes) = 
    Component "MState" True [sNetSetList, sNotes]
      where
        sNetSetList = Component "NetworkSet list" True (map showI netSetList)
        sNotes = Component "note list" True (map showI notes)

{-
showNetworkSet :: NetworkSet es -> String
showNetworkSet (NetworkSet nextT networks _) = error "showNetworkSet"
  where
    
-}

