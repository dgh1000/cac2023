{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, MultiParamTypeClasses,
             BangPatterns, TupleSections #-}

module Cac.Types where

import qualified Data.List as L
import qualified Data.Map as M
import Text.Printf
import System.Random
import Control.Arrow
import Control.Monad
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer.Strict
import Control.Monad.State.Strict
import Data.Monoid
import Data.Map(Map)
import Data.Set(Set)
import Cac
import Cac.Search
import Cac.Pcs
import Util.Showable

data Step1 = Step1 Int

data Comp1 = Comp1
  { c1Pits            :: [Int]
  , c1MinPit          :: Int
  , c1MaxPit          :: Int
  , c1TargetN         :: Int
  , c1Motif           :: Pcs
  , c1MotifSubsets    :: Map Int (Set Pcs)
  , c1FragSize        :: Int
  , c1Eval            :: [Comp1 -> Er1 Double]
  , c1Prune           :: [Comp1 -> Step1 -> Er1 Bool]
  }

----------------------------------------------------------------------

data Er1State = Er1State
  { e1sGen :: StdGen
  , e1sCache :: Map Pcs (Map Int [Pcs])
  }

type Er1 = ExceptT Exc (WriterT [ShowItem] (State Er1State))

newEr1State :: IO Er1State
newEr1State = do
  g <- newStdGen
  return $ Er1State g M.empty

runEr1 :: Er1State -> Er1 a -> ((Either Exc a,[ShowItem]),Er1State)
runEr1 g m = runState (runWriterT $ runExceptT m) g

evalEr1 :: Er1State -> Er1 a -> (Either Exc a,[ShowItem])
evalEr1 g = fst . runEr1 g


instance Ra Er1 where
  getGen = gets e1sGen
  putGen x = modify (\s -> s {e1sGen=x})

instance ExcClass Er1 where
  throwExc s = throwE $ Exc [s]
  catchExc = catchE
  withExc = withExceptT

instance Wr Er1 where
  tellSi s = lift (tell [s])


----------------------------------------------------------------------

instance Opt Comp1 Step1 Er1 where

  oComplete c = return $ length (c1Pits c) == c1TargetN c
  
  oList c     = do
    let steps1 = map Step1 [c1MinPit c..c1MaxPit c]
        runPrune s = do
          bools <- mapM (\g -> g c s) (c1Prune c)
          return (or bools,s)
    (map snd . filter (not . fst)) `liftM` mapM runPrune steps1

  oApply (Step1 s) c  = return c { c1Pits = c1Pits c ++ [s] }

  oEval c     = sum `liftM` mapM (\g -> g c) (c1Eval c)

  
----------------------------------------------------------------------

prune_octRel :: Int -> Comp1 -> Step1 -> Er1 Bool
prune_octRel nBack !c !s@(Step1 s1) = do
  result <- prune_octRel' nBack c s
  -- tellSi . SingleLine $
  --   printf "prune_octRel: called with step %d, result is %s" s1 (show result)
  return result

prune_octRel' nBack !c !(Step1 s) = do
  let ps = take nBack . reverse $ c1Pits c
      isOctRel x = x /= s && x `mod` 12 == s `mod` 12
  return $ any isOctRel ps
  
prune_rep :: Comp1 -> Step1 -> Er1 Bool
prune_rep !c !(Step1 s) = do
  result <- prune_rep' c (Step1 s)
  -- tellSi . SingleLine $
  --   printf "prune_rep: called with %d, result is %s" s (show result)
  return result
    
prune_rep' !c !(Step1 s) | null pits = throwExc "prune_rep: null comp"
                       | otherwise = return $ s == last pits
  where pits = c1Pits c


analyzeFrag_er1_1 :: Map Int (Set Pcs) -> Pcs -> Er1 (Map Int [Pcs])
analyzeFrag_er1_1 !subsets !frag = do
  cache <- gets e1sCache
  case M.lookup frag cache of
    Just x  -> return x
    Nothing -> do
      let a = analyzeFrag subsets frag
      modify (\s -> s {e1sCache = M.insert frag a cache})
      return a

analyzeFrag_er1_2 subsets frag = return $ analyzeFrag subsets frag


analyzeFrag_er1_3 :: Map Int (Set Pcs) -> Pcs -> Er1 (Map Int [Pcs])
analyzeFrag_er1_3 !subsets !frag = do
  cache <- gets e1sCache
  case M.lookup frag cache of
    Just x -> return x


eval_pcset :: [Double] -> Comp1 -> Er1 Double
eval_pcset !weights !c = do
  let pits = map head . L.group . reverse $ c1Pits c
  a <- analyzeFrag_er1_3 (c1MotifSubsets c) (pFromList $ take (c1FragSize c) pits)
  let f :: (Int,Int) -> Double
      f (subsetSize,subsetCount) = fromIntegral subsetCount * (weights !! subsetSize)
  return . sum . map f . M.toList . M.map length $ a


eval_rep :: Int -> [Double] -> Comp1 -> Er1 Double
eval_rep !repFragSize !weights !c = do
  let ps = take repFragSize . reverse $ c1Pits c
  case ps of
    [] -> throwExc "eval_rep: no pitches"
    (x:remain) -> do
      let count = length $ filter (==x) remain
      if count < length weights
        then return $ weights !! count
        else throwExc "eval_rep: weights list not big enough"

----------------------------------------------------------------------

instance ShowItemClass Comp1 where
  showI c = SingleLine . show $ c1Pits c



data PruneResult = PrNope String
                 | PrAccept


data EvalResult = EvalResult String Double


-- for every fitness function E, applied to all pitches, we see how it ranked those pitches
-- compared to how

-- we have fitness functions E_1, E_2, ... and we have steps S1, S2, ...
--
-- we have some tuples [(E,[(S,Ed)])]
--
-- then we have for each step: [(S,Ed)]


-- Fsr:  "fitness of step result"
data Fsr = Fsr String Double Int -- result of applying fitness function to one step: <fit fn
                                 -- name>, <fitness> <rank compared to same fitness function
                                 -- applied to other steps>

data Fsr2 = Fsr2 String Double  -- result of applying fitness function to one step, before the
                                -- rank has been computed

data AllFsr step = AllFsr step Double [Fsr]


data FitFn comp step m = FitFn String (comp -> step -> m Double)

data StepResult = StepResult Double Int [Fsr]

evalAll :: Opt comp step m => comp -> [FitFn comp step m] -> [step] -> m [AllFsr step]
evalAll c ffs ss = do
  -- ss :: [(step,(Double,[Fsr2]))]
  ss <- mapM (evalAll_funcs c ffs) ss
  -- now we need to produce [(step,Double)], and [(step,Fsr2)]
  return []



evalAll_funcs :: Opt comp step m => comp -> [FitFn comp step m] -> step ->
                 m (step,(Double,[Fsr2]))
evalAll_funcs c ffs step = do
  let g (FitFn name func) = do
        fit <- func c step
        return (Sum fit,[Fsr2 name fit])
  ((step,) . (getSum *** id) . mconcat) `liftM` mapM g ffs

{-
let applyFuncs :: [FitFn m] -> Step1 -> (Step1,[Ed])
      applyFuncs ffs step = do
        let applyF :: Step1 -> FitFn -> Er1 Ed
            applyF step (FitFn name f) = do
              fit <- f c step
              return $ Ed fit name 0
        ed <- mapM (applyF step) ffs
-}        
        
        

{-
computePrune :: [PruneResult] -> PruneResult
computePrune ps = case find f ps of
    Nothing -> PrAccept
    Just p  -> p
  where
    f (PrNope _) = True
    f PrAccept   = False
-}



-- computeEval :: (Int,[EvalResult]) -> (Double,[(Int,)
