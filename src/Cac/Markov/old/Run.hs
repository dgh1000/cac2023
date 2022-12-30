module Cac.Markov.Run where

import Cac.Markov.Data


-- 1. produce next note or notes from existing pointers
-- 2. extiguish pointers according to rules.
--
runOne :: M Note
runOne = do
  
  return $ Note 0.0 1.0 60 0.0 "default"
