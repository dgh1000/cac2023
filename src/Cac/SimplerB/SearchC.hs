
{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses,
             FunctionalDependencies, FlexibleContexts, ScopedTypeVariables #-}

module Cac.SimplerB.SearchC where

import qualified Data.List as L
import qualified Data.Map as M
import Debug.Trace
import Text.Printf
import Data.Map(Map)
import Cac.SimplerB.SimplerBData
import Data.Maybe
import Data.Monoid
import Data.Function
import Control.Lens
import Control.Lens.TH
import Control.Monad.State
import Control.Arrow
import Util.Exception
import Util.RandMonad
import Control.Monad
import Cac.SimplerB.Percentile

-- 'Ss' is state used for accumulating results of eval units run on a single
-- step.
--
--   'scores': holds score results for any eval units that are run and
--              (1) do apply and (2) don't prune
--
--   'notApplic' : holds scores that don't apply to this step
--                 case (the unit name and log might still be useful to know)
--   'mPruned'   : if one eval unit caused pruning, this holds that name
--
data Ss = Ss
  { _ssScores    :: [EvalUnitScore]
  , _ssNotApplic :: [EvalUnitScore]
  , _ssMPruned   :: Maybe EvalUnitScore
  }

type Tss = State Ss

makeFields ''Ss

addNBestSteps :: (CompClass c eu step) => Int -> c -> (SMonad c step) c
addNBestSteps n compIn = addNBestSteps' n compIn

  
addNBestSteps' :: (CompClass c eu step) => Int -> c -> (SMonad c step) c
addNBestSteps' nRemain cIn | nRemain <= 0 = return cIn 
                           | otherwise = do
                               cOut <- addBestStep cIn
                               addNBestSteps' (nRemain-1) cOut 


addBestStep :: (CompClass c eu step) => c -> (SMonad c step) c
addBestStep compIn = do
  let evalUnits = getEvalUnits compIn
      -- try step accumulates an EvalStepScore. is there anything along
      -- the way that we need to record?
      -- ess :: [EvalStepScore c step]
      ess = map (tryStep compIn) (genSteps compIn)
  pickBest ess
  
      
tryStep :: CompClass c eu step => c -> step -> EvalStepScore c step
tryStep comp step = toEvalStepScore cNew step accum
  where
    cNew = addStep comp step
    accum = snd $ runState (stateAccumSteps cNew (getEvalUnits comp))
                  (Ss [] [] Nothing)


-- pickBest
--
--   Given a list of EvalStepScores (representing the result of running the
--   eval unit set on each possible step), choose the best step randomly from
--   among all tied score. Construct a StepReport and 'put' it.
--
pickBest :: (CompClass c eu step, Ord step) => [EvalStepScore c step] ->
            (SMonad c step) c
pickBest stepScores =
  (do chosenStep <- rChooseList g1 
      let stepReport = StepReport po
                       chosenStep
                       (computeUnitPercentiles stepScores 
                         (view theStep chosenStep))
                       nNonPruned nLeastBadHalf nTied
      modify $ over reports (stepReport:)
      return $ view evaluatedComp chosenStep)
  where
    nonPruned = filter (isJust . view summedResult) stepScores
    nNonPruned = length nonPruned
    leastBadHalf = leastFractionOf 0.5 $ percentile nonPruned ssGetBadness
    nLeastBadHalf = length leastBadHalf
    -- this is assuming percentile groups 
    po@(PercentileOut percGroups _) = percentile leastBadHalf ssGetNegGoodness
    (score,g1) = case percGroups of
      x:_ -> x
      _ -> (printf ("Length percGroups: %d " ++
                   "nLeastBadHalf: %d")
                   (length percGroups) nLeastBadHalf) `trace` error "foo"
    nTied = length g1


leastFractionOf :: Double -> PercentileOut a -> [a]
leastFractionOf frac (PercentileOut _ percSingle) =
  case takeWhile ((<= frac) . fst) percSingle of
    [] -> map snd percSingle
    xs -> map snd xs

    
computeUnitPercentiles ::
  (CompClass c eu step, Ord step) =>
  [EvalStepScore c step] -> step -> Map String Double
computeUnitPercentiles ess stepIn = upm
  where
    upm = lookupUnitPerc stepIn $ unitPercentileMap $ buildUnitMap ess 


buildUnitMap ::
  (CompClass c eu step, Ord step) =>
  [EvalStepScore c step] -> Map String (Map step EvalUnitScore)
buildUnitMap = error "foo"


unitPercentileMap ::
  Ord step =>
  Map String (Map step EvalUnitScore) -> Map String (PercentileOut step)
unitPercentileMap = error "foo"


lookupUnitPerc ::
  Ord step => step -> Map String (PercentileOut step) -> Map String Double
lookupUnitPerc step = M.map (lookupPercentile step)

                    
ssGetNegGoodness :: CompClass c eu step => EvalStepScore c step -> Double
ssGetNegGoodness = (* (-1)) . fst . fromJust . view summedResult


ssGetBadness :: CompClass c eu step => EvalStepScore c step -> Double
ssGetBadness = snd . fromJust . view summedResult


{-
-- pickBest
--
--   we need to pick from ties randomly. anything else needed? we want to
--   record here any statistics on how often best ranked overall correlates
--   with best ranked by individual eval units
--
pickBest_orig :: [EvalStepScore c st] -> c
pickBest_orig ss = case ff3 of
    []                          -> throwMine "kl2nta"
    ((EvalStepScore _ _ _ c),_):_ -> c
  where
    ff = catMaybes $ map do1 ss
    do1 ss1@(EvalStepScore _ _ mSum _) = case mSum of
      Nothing    -> Nothing
      Just (g,b) -> Just (ss1,(g,b))
    ff2 = L.sortBy (compare `on` (snd . snd)) ff
    ff3 = if length ff2 >= 4
            then L.sortBy (compare `on` (fst . snd)) $
                   take (length ff2 `div` 2) ff2 
            else throwMine "not enough steps to choose from"
-}

-- stateAccumsteps
--
--   A State monad computation that accumulates results of running a number of
--   eval units on a particular step "step_eval"
--
--     'c': the state of the composition after step_eval has been added
--
--     [eu] : a list of eval units.
--
--   
stateAccumSteps :: CompClass c eu step => c -> [eu] -> State Ss ()
stateAccumSteps c [] = return ()
stateAccumSteps c (eu:eus) = do
  -- in the following line, sc is an EvalUnitScore:
  --
  --   if the particular eval unit is n/a, then field 'applic' will be False
  --
  --   if the eval unit prunes this step, then field 'scoreResult' will be
  --   Nothing
  --
  --   
  let sc = evalComp c eu 
  if view applic sc
    then modify (over notApplic (sc:))
    else
      if isJust (view result sc)
        then do
          modify (over scores (sc:))
          stateAccumSteps c eus
        else modify (set mPruned (Just sc))
    

-- toEvalStepScore
--
--   
toEvalStepScore :: c -> step -> Ss -> EvalStepScore c step
toEvalStepScore c st (Ss scs _ mPr) = case mPr of
      Nothing        -> EvalStepScore st scs Nothing (Just $ sumGoodBad scs) c
      Just thePruned -> EvalStepScore st (thePruned:scs) (Just thePruned)
                                      Nothing c
  where
    sumGoodBad :: [EvalUnitScore] -> (Double,Double)
    sumGoodBad scs = (xOut,yOut)
      where
        (Sum xOut,Sum yOut) = mconcat $ map (\(x,y) -> (Sum x, Sum y))
                              $ catMaybes $ map (view result) scs
