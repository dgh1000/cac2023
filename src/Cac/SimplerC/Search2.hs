{-# LANGUAGE ScopedTypeVariables #-}
module Cac.SimplerC.Search2 where

import Cac.SimplerC.Data
import Control.Monad.State
import Control.Lens

{-
-- what is CData?
data CData comp step = CData
  { _cDataComp    :: comp
  , _cDataChoices :: [Step comp step]
  , _cDataEus     :: [Eu comp step]
  , _cDataScores  :: [EvalUnitScore]
  , _cChoices     :: 
  }

type Cm comp step a = State (CData comp step) a
-}


type SimpleResult = (Integer,([EuResult],Maybe EuResult))

allStepResults :: forall comp step. C comp step -> [SimpleResult]
allStepResults cDataIn = map doStep1 chs
  where
    compIn = view comp cDataIn
    chs :: [step]
    chs = (view choicesFunc cDataIn) compIn
    eus :: [Eu comp step]
    eus = (view getEus cDataIn) compIn
    doStep1 :: step -> SimpleResult
    doStep1 st = ((view getStepId cDataIn) c2 st,(results,mPruneResult)) 
      where
        -- just add 
        -- oh we need to choose the results here, and make the non-raw results
        -- choosing results: the least bad half, and the best among the good.
        -- 
        -- some kind of search of results for one step. how do we
        --
        -- oh first we construct Results
        (results,mPruneResult) = doManyEu eus [] :: ([EuResult],Maybe EuResult)
        c2 = (view addStep cDataIn) compIn st
        --
        -- doManyEu
        --
        --   This will sort of "loop" via tail recursion.
        --
        doManyEu :: [Eu comp step] -> [EuResult] ->
                    ([EuResult], Maybe EuResult)
        doManyEu [] resultsIn = (resultsIn,Nothing)
        doManyEu (e:es) resultsIn = case result1 of
          ErPrune _ _ -> (resultsIn,Just result1)
          _           -> doManyEu es (result1:resultsIn)
          where
            result1 = e c2 st
         
