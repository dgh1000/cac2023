
module Cac.SimplerB.Eval01 where

import qualified Data.Map as M
import Data.Map(Map)
import Cac.SimplerB.SimplerBData
import Cac.SimplerB.Comp01
import Control.Lens
import Control.Lens.TH 
import Cac.Pcs



-- eval shape. timing. well there will be some patterns, like a motive spread
-- over time. if the motive is specific pitches, then it is unlikely to ever
-- fit. there are so many motives. longer motive, shape? interleaved motives?
-- a few pitches down and then back up? up/down? micro motives. leap down.
--
-- successive leap downs. chain them together. leap down step up. have balance
-- but not literal repetition. 
--
-- basic idea: motives, but flexible, and chained together in various ways.
--
-- motive: steps down, leaps up. could be 1 to 3 steps down, 1 to 3 leaps
-- up. that makes 9 combinations.
--
-- some notes closer together, some far apart. clumps. but hierarchical. So
-- we've got three levels of hierarchy, each with gaussian variation or some
-- such variation in delay. spans would be required as a kind of gaussian or
-- evenly spread distribution. We need "even distribution" class.
--
-- just not too much repetition either locally, or over larger span.
--


-- what should we do, look at pc sets in local region? every subset? our
-- analysis was with respect to a particular set. let's use analysis.

-- should config come along with Comp01? easy way to get started

evalPcs :: Comp01 -> EvalUnitScore
evalPcs comp
  | getCurrentStepCase comp == ScPitch = EvalUnitScore "pcs" "" out True
  | otherwise = EvalUnitScore "pcs" "not pitch case" (Just (0,0)) False
  where
    out =  Just $ evalPcs' (view config comp)
                  (take 8 (map (view pitch) $ view notes comp))


evalPcs' :: Comp01Config -> [Int] -> (Double,Double)
evalPcs' (Comp01Config subsets values) pitches =
    (sum $ map g $ M.toList result, 0)
  where
    result :: Map Int [Pcs]
    result = analyzeFrag subsets (pFromList pitches)
    -- lets say we have a 4-pc set, and we look at last seven pitches. every
    -- time the 4-set occurs scores a point. 3-sets score half a point 
    f :: Int -> Double
    f i = case lookup i [(4,1.0),(3,0.5),(2,0.3)] of Just d -> d
    g (n,ss) = fromIntegral (length ss) * f n


evalLoud :: Comp01 -> EvalUnitScore
evalLoud comp
  | getCurrentStepCase comp == ScLoud =
    EvalUnitScore "loud" "" (Just (1,1)) True
  | otherwise =
    EvalUnitScore "loud" "" Nothing False
