{-# LANGUAGE ScopedTypeVariables #-}

module Cac.Markov.TestData where 


import qualified Data.Map as M
import Data.Map(Map)
import Control.Monad.State
import Cac.Markov.Data
import Cac.Markov.Run
import Cac.Markov.Util (ListNote(..), ListNetworkSet(..))
import System.Random
import Control.Lens
import Util.RandMonad
import Util.Showable


main = do
  gen <- newStdGen
  let (_, MState _ netSets notes) = runM run01 (MState gen [initNetSet] [])
  writeFile "notes.txt"   $ showIString $ ListNote notes
  writeFile "netSets.txt" $ showIString $ ListNetworkSet netSets 

  
initNetwork :: String -> Network ()
initNetwork name = Network name nodeMap 1


initNetSet :: String -> NetworkSet ()
initNetSet name = NetworkSet 0.0 [initNetwork name] followFunc01

{-
stepStoreNote :: M () ()
stepStoreNote = do
  (n, newNetworkSet) <- step01
  modify
    (\s -> s { msNotes = n : msNotes s
             , msNetworkSetList = [newNetworkSet]
             })
  
-}

data FollowData = FollowData Int 


-- need to create test network

followFunc01 :: Network () -> Node () -> M () Int
followFunc01 _ node = do
  let folList = view followList node
  f <- rChooseList folList
  return $ view iden f    


makeNode :: Int -> [Int] -> (Int,Node ())
makeNode thisId followIds = (thisId, n)
  where
    n = Node
      { _nodeIden       = thisId
      , _nodePitch      = Just 60
      , _nodeTSpan      = Just 1.0
      , _nodeDur        = Just 0.5
      , _nodeAmpl       = Just (-10.0)
      , _nodeTimbre     = Just "default"
      , _nodeFollowList = map (flip Follow ()) followIds
      }


nodeMap :: Map Int (Node ())
nodeMap =
  M.fromList
    [ makeNode 1 [2, 3]
    , makeNode 2 [1, 3]
    , makeNode 3 [1, 2] ]

-- go :: NodeId -> MState NodeId

