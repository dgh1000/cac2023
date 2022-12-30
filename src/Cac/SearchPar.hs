{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveAnyClass,
    MultiParamTypeClasses, FunctionalDependencies, BangPatterns, DeriveGeneric #-}

module Cac.SearchPar where

import qualified Data.List as L
import qualified Data.Map as M
import GHC.Generics
import Text.Printf
import Data.Map(Map)
import Data.Function
import Data.List(sortBy)
import Data.Set(Set)
import Control.Parallel
import Control.Parallel.Strategies
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import Control.DeepSeq
import System.Random
import Util.Showable
import Cac
import Cac.Pcs
import Cac.Search

----------------------------------------------------------------------



-- haskell post

-- 

-- MyState is the data for the State monad. It serves three purposes:
--
-- (1) holds a System.Random generator
--
-- (2) caches computations that may come up repeatedly
--
-- (3) keeps some information about patterns within the search
--     for later analysis.
data MyState = MyState
             deriving(NFData,Generic)

type M = State MyState

-- Struct: data structure that I'm optimizing: It's composed of a Elems.
data Struct = Struct [Elem]
            deriving(NFData,Generic)


-- Elem: one element of a Struct
data Elem = Elem
          deriving(NFData,Generic)

-- compute list of elements that are possible as the next Elem to add to a Struct
listElems :: Struct -> [Elem]
listElems _ = error "foo"

-- divide up a list into a number of smaller lists of roughly equal size
divideUp :: Int -> [a] -> [[a]]
divideUp _ _ = error "foo"

-- compute a fitness function on a Struct. we will search for Struct with maximum fitness.
eval :: Struct -> Int
eval _ = error "foo"

-- add a new Elem to a Struct
addElemTo :: Struct -> Elem -> Struct
addElemTo _ _ = error "foo"

-- test if a Struct is full-sized (no more Elems can be added)
isFullSized :: Struct -> Bool
isFullSized _ = error "foo"


-- splitState: split MyState for several parallel computations.
--
-- This does the following:
--
--   (1) split the StdGen into N new generators with the System.Random
--       'split' function. (I realize this function is not a tested or
--       reliable source of pseudorandom numbers. I'll try to find a way
--       to address this in the next version.)
--
--   (2) replicate the cache and metrics without any changes
--
splitState :: Int -> MyState -> [MyState]
splitState _ _ = error "foo"


-- combine several MyStates into one
--
-- This combines the metrics and caches of each MyState to produce a new MyState. It picks one
-- of the StdGens arbitrarily.
combineStates :: [MyState] -> MyState
combineStates _ = error "foo"


-- searchPar: the optimization algorithm.
--
--   Here I split each level of search into 2 parallel computations. I don't think there's a
--   need to split into more than 2, as it will only take a few levels deep before there are
--   more computations than my computer's number of cores.
--
--   The result of calling this function is (<fitness of optimal Struct>, <optimal Struct>)
searchPar :: Struct -> M (Int,Struct)
searchPar struct
  | isFullSized struct = return (eval struct,struct)
  | otherwise = do
      currentState <- get
      let twoLists = divideUp 2 (listElems struct)
      
          -- descendM: within the M monad, try adding every input Elem to 'struct' in turn and
          -- recursively calling searchPar with the results. Return the best result.
          descendM :: [Elem] -> M (Int,Struct)
          descendM elems = do
            results <- mapM (searchPar . addElemTo struct) elems
            return $ L.maximumBy (compare `on` fst) results
            
          -- descend: run descendM
          descend :: MyState -> [Elem] -> ((Int,Struct),MyState)
          descend s elems = runState (descendM elems) s
          
          -- Within the Eval monad, call 'descend' several times in parallel. This is written
          -- to easily allow more than two parallel computations if that turns out to be
          -- advantageous later.
          (newState,bestResult) = runEval $ do
            let computations = zipWith descend (splitState 2 currentState) twoLists
            xs <- mapM (rpar . force) computations
            mapM rseq xs
            let (newStructs,newStates) = unzip xs
            return (combineStates newStates, L.maximumBy (compare `on` fst) newStructs)
      put newState
      return bestResult            
            

            
              
      
  


----------------------------------------------------------------------




