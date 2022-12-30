{-# LANGUAGE   TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies,
              FlexibleInstances #-}

module Cac.SimplerD.Data where

import Control.Monad.State
import Control.Lens.TH

type SeData = Integer

type Se = State SeData


data Time = Time
  { _timeTBeg :: Int
  , _timeTEnd :: Int
  }

makeFields ''Time

data Note = Note
  { _noteNTime    :: Time
  , _noteNPitch   :: Int
  , _noteNAmpl    :: Double
  }


makeFields ''Note


data Step = StepPit  Integer Int
          | StepSpan Integer Int
          | StepAmpl Integer Double
          deriving(Eq,Ord)

stepId :: Step -> Integer
stepId (StepPit  i _) = i
stepId (StepSpan i _) = i
stepId (StepAmpl i _) = i


data StepType = StepTypePit
              | StepTypeSpan
              | StepTypeAmpl


-- type Eu = Comp -> Step -> EuResult


-- Any EvalUnit will either be not applicable (such as testing rhythm in a
-- section about, will result in a prune (no way this can work)
data EuResult = ErNotApplic String Step
              | ErPrune     String Step
              | ErScore     String Step Double Double -- <good> <bad>


euResultStep (ErNotApplic _ i) = i
euResultStep (ErPrune     _ i) = i
euResultStep (ErScore     _ i _ _) = i


euResultName (ErNotApplic s _) = s
euResultName (ErPrune     s _) = s
euResultName (ErScore     s _ _ _) = s


euResultMaybeGood (ErScore _ _ g _) = Just g
euResultMaybeGood _ = Nothing


euResultMaybeBad (ErScore _ _ _ b) = Just b
euResultMaybeBad _ = Nothing


data StepResult =
  StepResultGoodBad
  { srStep    :: Step
  , srResults :: [EuResult]
  , srGoodSum :: Double
  , srBadSum  :: Double
  }
  |
  StepResultPruned
  { srStep      :: Step
  , srPruningEu :: EuResult }



data Eu = Eu
  { euRun      :: Comp -> Step -> EuResult
  , euRunPrune :: Comp -> Step -> Bool -- run in prune-only mode. True means
                                       -- pruned.
  }


-- choose in order
--
--   pitch
--   time
--   ampl
data Comp = Comp
  { _compNotes              :: [Note]
  , _compLastTime           :: Int
  , _compLastChosenPit      :: Int
  , _compLastChosenTime     :: Int
  , _compLastChosenStepType :: StepType
  -- for general-purpose algorithm
  , _compEvalUnits          :: [Eu]
  , _compListSteps          :: Comp -> [Step]
  , _compAddStep            :: Comp -> Step -> Comp
  }

makeFields ''Comp


data S = S
  { _sComp :: Comp }

makeFields ''S
