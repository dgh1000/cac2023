{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses,
             FunctionalDependencies #-}

module Cac.SimplerB.SimplerBData where

import Data.Map(Map)
import System.Random
import Control.Monad.State
import Cac.SimplerB.Percentile
import Control.Lens
import Control.Lens.TH
import Util.RandMonad


class Ord step => CompClass c eu step | c -> eu, c -> step where
  getEvalUnits :: c -> [eu]
  evalComp     :: c -> eu -> EvalUnitScore
  addStep      :: c -> step -> c
  genSteps     :: c -> [step]



{-
data Choice = ChPitch Int
            | ChTime  Double
            | ChDur   Double
-}

data ScoreResult = SrScored Double Double
                 | SrPruned
                 | SrNotApplic


data EvalUnitScore = EvalUnitScore
 { _evalUnitScoreName   :: String
 , _evalUnitScoreLog    :: String
 , _evalUnitScoreResult :: Maybe (Double,Double)
 , _evalUnitScoreApplic :: Bool
 }


makeFields ''EvalUnitScore

-- EvalStepScore
--
--   'theStep' : the step that's being evaluated
--
--   'unitScores': list of unit scores that were applic. if one of them
--                 pruned, this includes the pruned one
--
--   'thePruned' : if one of the eval units pruned, this is its unit score
--
--   'summedResult': if none of the eval units pruned, this is the sum
--                   of all good and the sum of all bad
--
--   'evaluatedComp' : the comp with the "step being evaluated" applied
--                 
data EvalStepScore c step = EvalStepScore
  { _evalStepScoreTheStep       :: step 
  , _evalStepScoreUnitScores    :: [EvalUnitScore]
  , _evalStepScoreThePruned     :: Maybe EvalUnitScore
  , _evalStepScoreSummedResult  :: Maybe (Double,Double)
  , _evalStepScoreEvaluatedComp :: c
  }

makeFields ''EvalStepScore

instance Show (EvalStepScore a b) where
  show _ = ""
  

data StepReport c step = StepReport
  { _stepReportStepScores     :: PercentileOut (EvalStepScore c step)
  -- , _stepReportEvalUnitScores :: Map String (PercentileOut EvalUnitScore)
  , _stepReportTheChosen         :: EvalStepScore c step
  , _stepReportEvalUnitPerc   :: Map String Double
  , _stepReportNNonPruned     :: Int
  , _stepReportNLeastBadHalf  :: Int
  , _stepReportNTied          :: Int
  }

makeFields ''StepReport

data SearchState c step = SearchState
  { _searchStateGen     :: StdGen
  , _searchStateReports :: [StepReport c step]
  }

makeFields ''SearchState  

type SMonad c step = State (SearchState c step)


instance RandMonad (SMonad c step) where
  putGen g = modify (set gen g)
  getGen = gets (view gen) 


class Monad m => SearchMonad m where
  putReport :: StepReport comp step -> m ()


