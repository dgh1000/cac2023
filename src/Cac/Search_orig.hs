{-# LANGUAGE TypeSynonymInstances, FlexibleInstances,
    MultiParamTypeClasses, FunctionalDependencies, BangPatterns #-}

module Cac.Search where

import qualified Data.List as L
import qualified Data.Map as M
import Text.Printf
import Data.Map(Map)
import Data.Function
import Data.List(sortBy)
import Data.Set(Set)
import Control.Monad
import Control.Monad.State.Strict
import Control.Monad.Trans.Except
import System.Random
import Util.Showable
import Cac
import Cac.Pcs



class (ShowItemClass comp, Ra m, ExcClass m) => Opt comp step m | comp -> step, comp -> m where
  oComplete :: comp -> m Bool
  oList     :: comp -> m [step]
  oApply    :: step -> comp -> m comp
  oEval     :: comp -> m Double


data OptConfig = OptConfig
  { ocDescentDepth       :: Int
  , ocDescentWidth       :: Double
  , ocDescentCount       :: Int
  }


-- search
--
-- terms:
--
--   immediate fitness: the fitness returned by running oEval on a comp
--
--   descent fitness: we have a comp C. we add N steps to it in a lot of different random ways,
--     giving a bunch of compositions C_m. The maximum fitness of any C_m is the descent
--     fitness of C.
--
-- search by sort-of-Monte-Carlo. the top level routine here will add steps one-by-one until
-- the composition is constructed. at each step the following happens:
--
--   - we have an input composition C
--
--   - if it's complete, return C
--   
--   - make a list of possible next steps S_i
--
--   - for each step S
--
--     - apply S to C, and compute immediate fitness. we need this to pass to descentEval
--
--     - compute the descent fitness of C++S via descent eval
--
--   - pick the step S_m with the maximum descent eval. In case of tie, pick one randomly
--
--   - apply S_m and call 'search' recursively
--

search :: Opt comp step m => OptConfig -> comp -> m comp
search conf c = withExc (addContext report) (search' conf c)
  where
    report = showiToString $ Component "search: called with " True [showI c]

search' conf !c = do
  flag <- oComplete c
  if flag
    then return c
    else do
      steps <- oList c
      -- the goal of 
      let applyOne !s = do
            c2 <- oApply s c
            fit <- oEval c2
            dfit <- descentEval conf fit c2 0
            return (dfit,c2)   -- return (<descent fit of c2>,<c2 for handly use later>)
      results <- mapM applyOne steps
      raMaxRandTied fst results >>= search conf . snd


-- descentEval
--
-- Compute the descent fitness of a composition 'c'.
descentEval :: Opt comp step m => OptConfig -> Double -> comp -> Int -> m Double
descentEval !conf !currFit !c !level
  | level >= ocDescentDepth conf = return currFit
  | otherwise = do
      -- First we must make a list of steps to try for descending further.
      steps <- oList c
      let f !s = do
            c2  <- oApply s c
            fit <- oEval c2
            r <- raRandomR (0,1::Double)
            return ((fit,r),c2)
      -- results :: [((<immediate fitness>,<random 0 to 1>),<comp with step applied>)]
      -- (results will be sorted in order of descending fitness) 
      results <- (reverse . L.sortBy (compare `on` fst)) `liftM` mapM f steps
      -- Compute the "width" of this descent phase as an integer, called W.
      let l = length results
          w :: Int
          w = round $ ocDescentWidth conf * fromIntegral l
      when (w < 2) (throwExc "in descentEval2, integeter descent width < 2")
      -- 
      let stepSelection = take w results
          -- now must figure out how many possibilities we try from within the descent
          -- width. it will be the minimum of the integral size of the descent width and the
          -- configured descent count.
          nTry = min (ocDescentCount conf) w
      chosen <- raChooseN nTry stepSelection
      -- 
      let descend ((!fit,_),!c) = do
            deepFit <- descentEval conf fit c (level+1)
            return (deepFit,c)
      (finalFit,_) <- L.maximumBy (compare `on` fst) `liftM` mapM descend chosen
      return finalFit


----------------------------------------------------------------------

