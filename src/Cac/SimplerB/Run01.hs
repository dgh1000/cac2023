{-# LANGUAGE MultiParamTypeClasses, TemplateHaskell, FlexibleInstances,
             FunctionalDependencies #-}

module Cac.SimplerB.Run01 where

import qualified Data.Map as M
import System.Random
import Control.Monad.State
import Cac.SimplerB.SimplerBData
import Cac.SimplerB.Comp01
import Cac.SimplerB.Eval01
import Cac.Pcs
import Cac.SimplerB.SearchC
import Cac.SimplerB.ShowSimplerB
import Control.Lens
import Text.Printf
import Util.RandMonad
import Util.Showable

{-
data RsState c step = RsState
  { _rsStateGen     :: StdGen
  , _rsStateReports :: [StepReport c step]
  }


makeFields ''RsState

  
type MySearch c step = State (RsState c step)

instance RandMonad (MySearch c step) where
  getGen = gets (view gen)
  putGen g = modify (set gen g)

instance SearchMonad (MySearch c step) where
  putReport r = modify (\s -> s { _rsStateReports = r : _rsStateReports s})
-}

compA_firstPitches = [60,63,65,68]

compA_mkNote p = Note 0 1 p 5

compA_notes = map compA_mkNote compA_firstPitches
              

compA_config = Comp01Config ss M.empty
  where
    ss = analyzeSubsets $ pFromList [11, 0, 2, 3, 7]


compA = Comp01 { _comp01Notes     = compA_notes
               , _comp01LastT     = 3
               , _comp01RecentPcs = pFromList compA_firstPitches
               , _comp01NextPitch = Nothing
               , _comp01NextSpan  = Nothing
               , _comp01NextDur   = Nothing
               , _comp01Config    = compA_config
               , _comp01EvalUnits = [evalPcs] }



instance CompClass Comp01 EvalUnit01 Step01 where 
  evalComp = evalComp01
  addStep  = addStep01
  genSteps = genSteps01
  getEvalUnits = view evalUnits

main = do
  gen <- newStdGen
  let initialState = SearchState gen []
      (cOut,SearchState _ reports) = runState (addBestStep compA) initialState
      oneNote :: Note -> String
      oneNote (Note _ _ pit _) = printf "pit: %d pc: %d" pit (pit `mod` 12)
  putStrLn $ showIString cOut
