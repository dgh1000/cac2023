{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, DeriveAnyClass,
    MultiParamTypeClasses, FunctionalDependencies, BangPatterns, DeriveGeneric #-}

module Cac.Search where

import qualified Data.List as L
import qualified Data.Map as M
import GHC.Generics
import Text.Printf
import Data.Monoid
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

----------------------------------------------------------------------

-- haskell post

-- 


            
              
      
  


----------------------------------------------------------------------



class CustomState st where
  combineState2 :: [st] -> st


class (NFData comp, NFData step, NFData st, ShowItemClass comp, CustomState st) =>
    Opt comp step st | comp -> step, comp -> st, st -> comp where
  oComplete     :: comp -> Es st Bool
  oList         :: comp -> Es st [step]
  oApply        :: step -> comp -> Es st comp
  oEval         :: comp -> Es st Double
  oSplitState   :: Int -> Es st [st]


data OptConfig = OptConfig
  { ocDescentDepth       :: Int
  , ocDescentWidth       :: Double
  , ocDescentCount       :: Int
  }


-- SEARCH CONCEPTS
--
-- TERMINOLOGY:
--
--   'immediate fitness': the fitness of a comp returned by running oEval on it
--
--   'descent fitness':
--
--      we have a comp C. we add N steps to it in a lot of different random ways, giving a
--      bunch of compositions C_m. The maximum fitness of any C_m is the descent fitness of C.
--
-- search by sort-of-Monte-Carlo.
--
--   summary of algorithm:
--
--     "add steps one-by-one until the composition is finished."
--
--   what does it mean to "add a step"? follow this procedure:
--
--     - we have an input composition C
--
--     - if it's complete, return C
--   
--     - otherwise make a list of possible next steps S_i
--
--     - for each step S_i
--
--       - apply S_i to C, and call it CS. compute immediate fitness of CS.
--
--       - compute the descent fitness CS via descentEval, passing to it both CS and the
--         immediate fitness of CS.
--
--     - in the above loop, accumulate the list of steps (S_i, <descent fitness of S_i added to
--       C)
--
--     - pick the step S_m with the maximum descent eval. In case of tie, pick one randomly
--
--     - add that step S_m to C. Done with one round of "adding a step"
--
search3 :: Opt comp step st => Int -> OptConfig -> comp -> Es st comp
search3 numCores conf cIn = do
  flag <- oComplete cIn
  if flag
    then return cIn
    else do
      when (numCores < 1 || numCores > 4) (throwExc "invalid numCores, should be 1 to 4")
      steps <- oList cIn
      let subsets = divideListN numCores steps
      customStates <- oSplitState numCores
      gens <- splitGen numCores `liftM` getGen
      --  oneThreadEs: does work that can be split off in a single thread.
      --     it takes a subset of 'steps' (the possible next steps) and computes the
      --     descent fitness of each when added to the comp 'cIn'
      --  oneThreadEs :: [step] -> Es [(double,comp)]
      let oneThreadEs subsetSteps = do
            -- compute descent eval for one step 's'
            --  oneStep :: step -> Es (Double,comp)
            let oneStep s = do
                  cOut    <- oApply cIn s
                  currFit <- oEval cOut
                  fit <- descentEval conf currFit cOut 1
                  return (fit,cOut)
            mapM oneStep subsetSteps
          -- oneThread: a pure function that takes a st (custom state), a StdGen, and a subset
          --            of steps, and does descent eval
          -- oneThread :: [step] -> StdGen -> st -> ([(Double,comp)],st)
          oneThread subsetSteps oneGen oneState = do
            runEs (oneThreadEs subsetSteps) oneGen oneState
      {-
      runEval $ do
        let computations = zipWith3 oneThread subsets gens customStates
        return 0
      -}
      return cIn

oneThreadEs :: Opt comp step st => OptConfig -> comp -> [step] -> Es st [(Double,comp)]
oneThreadEs conf comp steps = error "oneThreadEs"

oneStepDescentEval :: Opt comp step st => OptConfig -> Double -> comp -> Int -> Es st Double
oneStepDescentEval conf currFit comp level = error "oneStepDescentEval"

type EsOut comp step st = (Either Exc [(Double,comp)],EsState st)

oneThread :: Opt comp step st => OptConfig -> comp -> [step] -> StdGen -> st ->
             EsOut comp step st
             
oneThread conf comp steps gen state = runEs (oneThreadEs conf comp steps) gen state

threads :: Opt comp step st => OptConfig -> comp -> [[step]] -> [StdGen] -> [st] ->
                      [EsOut comp step st]
threads conf comp subsetLists gens states =
  zipWith3 (oneThread conf comp) subsetLists gens states

evalComputation :: Opt comp step st => OptConfig -> comp -> [[step]] -> [StdGen] -> [st] ->
                   Eval (Either Exc ((Double,comp),(StdGen,st)))
evalComputation conf comp subsetLists gens states = do
  -- cs :: [EsOut comp step st]
  cs <- mapM (rpar {- . force -})  (threads conf comp subsetLists gens states)
  mapM_ rseq cs
  -- any left?
  let getLeft (Left e,_) = Just e
      getLeft _          = Nothing
  case getFirst . mconcat $ map (First . getLeft) cs of
    Nothing -> error "foo"
    Just e  -> return $ Left e
  
    
    

  


splitGen :: Int -> StdGen -> [StdGen]
splitGen n gen = [gen]

search2 :: Opt comp step st => Int -> OptConfig -> comp -> Es st comp
search2 numCores conf cIn = do
  flag <- oComplete cIn
  if flag
    then return cIn
    else do
      when (numCores /= 4) (throwExc "invalid numCores, should be 4")
      steps <- oList cIn
      let subsets = divideListN numCores steps
      g0 <- getGen
      let gs = take numCores $ L.unfoldr (Just . split) g0
          -- f (Either Exc (<comp>),<custom state)
          f gen subset = let m step = do
                               c2 <- oApply step cIn
                               fit <- oEval c2
                               dfit <- descentEval conf fit c2 0
                               return (dfit,c2)
                             m2 = do
                               xs <- mapM m subset
                               return . snd $ L.maximumBy (compare `on` fst) xs
                         in runEs m2 gen
          x = runEval $ do
             res0 <- rpar $ force $ f (gs!!0) (subsets!!0)
             res1 <- rpar $ force $ f (gs!!1) (subsets!!1)
             res2 <- rpar $ force $ f (gs!!2) (subsets!!2)
             res3 <- rpar $ force $ f (gs!!3) (subsets!!3)
             rseq res0
             rseq res1
             rseq res2
             rseq res3
             return [res0,res1,res2,res3]
      return cIn
{-      
      -- the goal of 
      let applyOne !s = do
            c2 <- oApply s c
            fit <- oEval c2
            dfit <- descentEval conf fit c2 0
            return (dfit,c2)   -- return (<descent fit of c2>,<c2 for handly use later>)
      results1 <- rpar $ force $ mapM applyOne steps1
      results2 <- rpar $ force $ mapM applyOne steps2
      l3 $ rseq results1
      l3 $ rseq results2
      -- let combined = force results1 `par` (force results2 `pseq` (results1++results2))
      ersMaxRandTied fst (results1++results2) >>= search conf . snd
-}


divideListN :: Int -> [a] -> [[a]]
divideListN n _ | n < 1 = error "divideListN called with n < 0"
divideListN 1 xs = [xs]
divideListN n xs = first : divideListN (n-1) last
  where
    (first,last) = splitAt (length xs `div` n) xs

{-
search :: Opt comp step st => OptConfig -> comp -> Ers st comp
search conf c = withExc (addContext report) (search' conf c)
  where
    report = showiToString $ Component "search: called with " True [showI c]



search' :: Opt comp step st => OptConfig -> comp -> Ers st comp
search' conf !c = do
  flag <- oComplete c
  if flag
    then return c
    else do
      steps <- oList c
      let (steps1,steps2) = divideListInTwo steps
      -- the goal of 
      let applyOne !s = do
            c2 <- oApply s c
            fit <- oEval c2
            dfit <- descentEval conf fit c2 0
            return (dfit,c2)   -- return (<descent fit of c2>,<c2 for handly use later>)
      results1 <- rpar $ force $ mapM applyOne steps1
      results2 <- rpar $ force $ mapM applyOne steps2
      l3 $ rseq results1
      l3 $ rseq results2
      -- let combined = force results1 `par` (force results2 `pseq` (results1++results2))
      ersMaxRandTied fst (results1++results2) >>= search conf . snd
-}


divideListInTwo :: [a] -> ([a],[a])
divideListInTwo [] = ([],[])
divideListInTwo xs = (take l xs,drop l xs) where l = length xs `div` 2


-- descentEval
--
-- Compute the descent fitness of a composition 'c'.
--
-- Parallel computation ideas: we split available choices into 4 and do descent evaluation
-- on all of them. this function calls itself recursively, but will stay in the state
-- monad at all times so I think it's fine.

descentEval :: Opt comp step st => OptConfig -> Double -> comp -> Int -> Es st Double
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
      let descend ((!fit,_),!c) = do
            deepFit <- descentEval conf fit c (level+1)
            return (deepFit,c)
      (maximum . map fst) `liftM` mapM descend chosen
      {-
      let (chosen1,chosen2) = divideListInTwo chosen
          descend ((!fit,_),!c) = do
            deepFit <- descentEval conf fit c (level+1)
            return (deepFit,c)
      results1 <- mapM descend chosen1
      results2 <- mapM descend chosen2
      let combined = force results1 `par` (force results2 `pseq` (results1++results2))
          (finalFit,_) = L.maximumBy (compare `on` fst) combined
      -- (finalFit,_) <- L.maximumBy (compare `on` fst) `liftM` mapM descend chosen
      return finalFit
      -}

----------------------------------------------------------------------

