module Util.ErrorRand(ErrorRand, runER, evalER, freshRandData) where

import Util.RandomState
import Control.Monad.State
import Control.Monad.Except

type ErrorRand a = ExceptT String (State RandData) a

runER m s = runState (runErrorT m) s
evalER m s = fst $ runER m s

