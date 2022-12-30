
module Cac.Eval where

import Cac.Comp

-- evaluation would focus on rhythms and gestures and repeated notes. we could
-- start with repeated notes. we have several "voices" which are drawing
-- information from a background of notes.
--
-- not too much repetition and with other pitch in-between?
--

evalRep1 :: Int -> Comp -> Double
evalRep1 pc (Comp ns) = error "evalRep1"
  where
    ps = evalRep1_getPitches pc ns :: [Int]


 
