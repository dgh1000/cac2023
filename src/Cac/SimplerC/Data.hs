{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses,
             FunctionalDependencies #-}

module Cac.SimplerC.Data where

import Control.Lens.TH
import Cac.SimplerC.Percentile
import qualified Data.Map as M
import Data.Map(Map)


data Comp = Comp [Integer]

data EuResult = ErNotApplic String Integer
              | ErPrune     String Integer
              | ErPositive  String Integer Double
              | ErNegative  String Integer Double


-- a function that evaluates one step using one algorithm 
type Eu comp step = comp EuResult

data C comp step = C
  { _cTheComp       :: comp Comp
  , _cChoicesFunc   :: comp [step]
  , _cGetEus        :: comp [Eu comp step]
  , _cAddStep       :: step -> comp ()
  , _cGetStepId     :: step -> comp Integer
  }


makeFields ''C





-- LEFT OFF : decide if this is complete
data RawResults = RawResults
 -- all EuResults for all eval units will be included in 'byUnitName'
 -- and 'byStepId' (including pruned and not applicable)
 -- okay later we need to pick best step: byStepId can be mapped to
 -- scores, twice (one for positive, one for negative) and percentile found
 --

 --   first remove all steps that are pruned by at least one result
 -- 
 --  Map Integer Double for all positive 
 { _rawResultsByUnitName  :: Map String (Map Integer EuResult)
 , _rawResultsN1    :: Map Integer ([EuResult],Maybe EuResult)
 }


makeFields ''RawResults 

-- LEFT OFF
-- results for each step: all units evaluate? get sums then rank from stepSums?
--
-- pick
--
-- run only pruning units in "quick mode"
--
data Results = Results
  -- results for 'byUnitName' will not include any steps that were
  -- pruned or for which the unit returned 'not applicable'
  { _resultsByUnitName :: Map String (Map Integer EuResult)
  -- results for 'byStepId' will not include any steps that were pruned
  , _resultsByStepId   :: Map Integer (Map String EuResult)
  -- step sums will include, obviously, only steps that were not pruned
  , _resultsStepSums   :: Map Integer (Double,Double)
  }

makeFields ''Results 

