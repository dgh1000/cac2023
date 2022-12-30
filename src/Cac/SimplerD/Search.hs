{-# LANGUAGE TupleSections #-}

module Cac.SimplerD.Search where


import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Arrow
import Data.Map.Strict(Map)
import Data.Set(Set)
import Data.Maybe
import Data.Either
import Cac.SimplerD.Data
import Control.Lens
import Cac.SimplerD.Percentile
import Util.Exception


-- general search algorithms
--
--   Result of running an eval unit:
--
--     (1) pruning
--
--     (2) not applicable: this unit doesn't apply to this type of step
--
--     (3) "positive" and "negative" values:
--
--           an eval unit determines both the presence of desirable qualities
--           (such as containing a pitch-class set we want to have) and the
--           presence of undesirable qualities (such as pitch-class sets we
--           don't want to have)
--
--
--   Could a given eval unit every produce both desirable and undesirable
--   qualities? for example we could have one that does an analysis of sorts,
--   and data from the analysis can be used for both purposes.
--
--

-- evaluate a step
--
--   how to choose by "discarding worst half (by bad score)" and "choosing
--   most good"
--
--
--
--
--
--
--


-- addBestStep
--
--   Given [a Comp] find and add [the best of the available next steps]
--
--   Some considerations:
--
--     1. There may be more than one step tied for best. (For now, choose
--     p-randomly.)
--
--     2. There may be no steps available--all are pruned.
--
--     
addBestStep :: Comp -> Se Comp
addBestStep c = do
  let -- Find all eval units associated with this comp.
      eus :: [Eu]
      eus = view evalUnits c
      -- Define a function for running all eus on this comp.
      runAllEus :: Step -> [EuResult]
      runAllEus step = map (\(Eu f _) -> f c step) eus
      ss :: Step -> (Step,Either EuResult [EuResult])
      ss step = (step,stepsUntilPruned c step)
      -- we need to choose steps not pruned
      steps :: [Step]
      steps = view listSteps c c
      rs :: [(Step,Either EuResult [EuResult])]
      rs = map ss steps
      f1 :: (Step,Either EuResult [EuResult]) -> Maybe (Step,[EuResult])
      f1 (_,Left _)        = Nothing
      f1 (stepIn,Right rs) = Just (stepIn,rs)
      list2 :: [(Step,[EuResult])]
      list2 = mapMaybe f1 rs
      f2 :: (Step,[EuResult]) -> (Step,(Double,Double))
      f2 (s,rs) = (s,sumResults rs)
      list3 :: [(Step,(Double,Double))]
      list3 = map f2 list2
      m3 :: Map Step (Double,Double)
      m3 = M.fromList list3
      leastBad = takeLeastBad m3
      -- m3_leastBad = M.filterWithKey (\k _ -> S.member k leastBad) m3

        
  error "e17483"


takeLeastBad :: Map Step (Double,Double) -> Map Step (Double,Double)
takeLeastBad input = M.filterWithKey (\k _ -> S.member k bs) input
  where
    input2 :: Map Step Double
    input2 = M.map (\(x,y) -> -y) input
    p :: Map Step Perc
    p = computePercentile input2
    bs :: Set Step
    bs = S.fromList $ chooseBest 0.5 p

-- addBestStep
--
--   list and add steps, run eus on result
--
--   choose be
addBestStep2 :: Comp -> Se Comp
addBestStep2 comp = error "fok3iubv4"
  where
    f :: Step -> StepResult
    f st = toStepResult $ stepsUntilPruned comp st
    srs :: [StepResult]
    srs = map f $ view listSteps comp comp 
    maybeNotPruned :: StepResult -> Maybe (Step,(Double,Double))
    maybeNotPruned (StepResultPruned       _ _) = Nothing
    maybeNotPruned (StepResultGoodBad st _ g b) = Just (st,(g,b))
    notPrunedList :: [(Step,(Double,Double))]
    notPrunedList = mapMaybe maybeNotPruned srs
    mGood :: Map Step Double
    mGood = M.fromList $ map (second fst) notPrunedList
    mBad  :: Map Step Double
    mBad  = M.fromList $ map (second snd) notPrunedList
    pGood :: Map Step Perc
    pGood = computePercentile mGood
    pBad  = computePercentile mBad
    
    {-
    -- EuResult. collate them? Percentile?
    f :: Step -> [EuResult]
    f st = map (\eu -> euRun eu comp st) (view evalUnits comp)
    -}

{-
type ResultsByStep = Map Step [EuResult]
type ResultsByEu   = Map String [EuResult]


allStepsAllEus :: Comp -> [Step] -> ResultsByStep
allStepsAllEus comp steps = m1
  where
    goStep :: Step -> (Step,[EuResult])
    goStep st = ( st
                , map (\eu -> euRun eu comp st) (view evalUnits comp) )
    m1 :: Map Step [EuResult]
    m1 = M.fromList $ map goStep steps
    m2 :: [EuResult]
    m2 = concat $ M.elems m1
    m3 :: EuResult -> (String,[EuResult])
    m3 r = (euResultName r,[r])
    m4 :: Map String [EuResult]
    m4 = M.fromListWith (++) $ map m3 m2
-}


stepsUntilPruned :: Comp -> Step -> Either EuResult [EuResult]
stepsUntilPruned comp step =
  stepsUntilPruned' comp step (view evalUnits comp) []


stepsUntilPruned' :: Comp -> Step -> [Eu] -> [EuResult] ->
                     Either EuResult [EuResult]
stepsUntilPruned' _ _ [] rs = Right rs
stepsUntilPruned' comp step (euIn:euRemain) rs = out
  where 
    out = case euRun euIn comp step of
      r@(ErPrune     _ _) -> Left r
      r@(ErNotApplic _ _) -> stepsUntilPruned' comp step euRemain rs
      r                   -> stepsUntilPruned' comp step euRemain (r:rs)
      
    
toStepResult :: Either EuResult [EuResult] -> StepResult
toStepResult (Left r) = StepResultPruned (euResultStep r) r
toStepResult (Right rs) = case rs of
  [] -> throwMine "jf9a83jvals"
  rs -> let (goodSum,badSum) = sumResults rs
            r:_ = rs
        in StepResultGoodBad (euResultStep r) rs goodSum badSum


sumResults :: [EuResult] -> (Double,Double)
sumResults rs = (sum $ map fst goodBads,sum $ map snd goodBads)
  where
    goodBads = map toGoodBad rs
    toGoodBad r = case r of
      ErScore _ _ g b -> (g,b)
      _               -> throwMine "84767"
    

-- organizing eval unit results
--
-- overview
--
--   we have a set of next steps S.
--
--   we have a set of eval units EU.
--
--   there's an outcome for every pair: O(S_m,EU_n)
--
--     outcome is "not relevant", "pruned", or a fitness score in terms of
--     "goodness" and "badness" (GB outcome)
--
--
-- how to organize the data
--
--   for every step S_m, we have either
--
--       - an indication it was pruned by EU_pr
--
--       - a list of GB outcomes for any applicable EU: LGB
--
--          - if we have this, we also have sum of all goodness numbers in LGB
--            and sum of all badness numbers in LGB
--
--
--       - this is represented by one 'StepResult' data
--
--



-- =================================================================
-- =================================================================
-- ==== important: this is first way of organizing results ======
--
--   after running every step we have:
--
--     Map Step StepResult
--
--     Map Step (Perc,Perc)  -- percentile of that step's goodness among
--                           -- good result, percentiles of that step's
--                           -- badness among bad results
--
-- =================================================================
-- =================================================================



-- how to select the winning step
--
--   if we are applying look-ahead of some sort, we have a collective good and
--   bad from a number of steps looked ahead.
--
--     how compute collective good and bad? good could be sum of goodness,
--     while bad is the sum of most 3 bad, or just most bad.
--
--   if we use Monte Carlo, then we have many collective good/bad. we could
--   sum or average good, while taking worse bad
--
--   okay, given that we have a set of steps and a collective good/bad for
--   each, we then discard the worst 75% bad, and take the most good of the
--   remaining, choosing randomly among ties.
--
-- monitoring each EU unit performance
--
--   we'd like to know if an EU makes any contribution to the chosen step.
--
--   we have set of steps S. we have chosen step S_c.
--
--   for each EU_i among set(EU):
--
--     create list of scores, EU_i_scores, with the result of running that EU
--     on every step.
--
--     G1 is goodness that EU_i gave S_c. Find percentile of G1 among
--     goodnesses that EU_i gave all steps.
--
--     B1 is badness that EU_i gave S_c. Find percentile of G1 among badness
--     that EU_i gave all steps.
--
--
--   we'll say that "EU_i *contributed* to the result" if G1 is a high
--   percentile and B1 is a low percentile.
--
--   then the question is, how often did EU_i "contribute"? if *never*, then
--   we might need to increase the weight on EU_i.
--
-- problems with "monitoring EU performance" when decisions are made by
-- "collective performance"
--
--   


----------------------------------------------------------------------
-- proceed simply, Nov 30, 2018
--
--   



-- percentileByStep
--
stepPercentiles :: [EuResult] -> Map Step (Perc,Perc)
stepPercentiles rs = M.intersectionWith (,) (go euResultMaybeGood)
                                            (go euResultMaybeBad)
  where
    {-
    toGood :: EuResult -> Maybe (Integer,Double)
    toGood r = case euResultMaybeGood r of
      Nothing -> Nothing
      Just g  -> Just (euResultStepId r,g)
    toBad :: EuResult -> Maybe (Integer,Double)
    toBad r = case euResultMaybeBad r of
      Nothing -> Nothing
      Just b  -> Just (euResultStepId r,b)
    -}
    go :: (EuResult -> Maybe Double) -> Map Step Perc
    go g = computePercentile m
      where
        m = M.fromList $ mapMaybe toNum rs
        toNum :: EuResult -> Maybe (Integer,Double)
        toNum r = fmap (euResultStep r,) $ g r


-- percentileByEu
--
--   where did the eu rank the winning step, or any particular step?
-- 
percentileByEu :: Integer -> [EuResult] -> Map String Perc
percentileByEu stepId rs = error "foo"
