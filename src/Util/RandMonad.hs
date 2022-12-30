{-# LANGUAGE GeneralizedNewtypeDeriving, FlexibleContexts,
             TypeSynonymInstances, FlexibleInstances #-}

module Util.RandMonad where

import Control.Monad.State
import System.Random
import Data.List
import Data.Function

----------------------------------------------------------------------
----------------------------------------------------------------------
--             a very simple instance of RandMonad

type RMon = State StdGen

instance RandMonad RMon where
  putGen   = put
  getGen   = get

runRMon :: RMon a -> StdGen -> (a,StdGen)
runRMon = runState



----------------------------------------------------------------------
----------------------------------------------------------------------


----------------------------------------------------------------------
--                    RandMonad


class Monad m => RandMonad m where
  putGen :: StdGen -> m ()
  getGen :: m StdGen
 
rRandom :: (Random r, RandMonad m) => m r
rRandom = do
  st <- getGen
  let (value, g') = random st
  putGen g'
  return value


rRandomR :: (Random r, RandMonad m) => (r,r) -> m r
rRandomR (lo, hi) = do
  g <- getGen
  let (value, g') = randomR (lo, hi) g
  putGen g'
  return value


rRandoms :: (Random r, RandMonad m) => m [r]
rRandoms = do
  g <- getGen
  let (g1, g2) = split g
  let values = randoms g1
  putGen g2
  return values


rRandomRs :: (Random r, RandMonad m) => (r,r) -> m [r]
rRandomRs (lo,hi) = do
  g <- getGen
  let (g1, g2) = split g
  let values = randomRs (lo,hi) g1
  putGen g2
  return values

rChooseList :: (RandMonad m) => [a] -> m a
rChooseList xs = do
  let l = length xs
  when (l==0) (error "in rChooseList, passed null list")
  idx <- rRandomR (0,l-1)
  return $ xs !! idx


rPermuteList :: (RandMonad m) => [a] -> m [a]
rPermuteList xs = do
  let l = length xs
  rs <- replicateM l (rRandomR (0,1::Double))
  let rs2 = sortBy (compare `on` snd) (zip [(0::Int)..] rs)
  return $ map (xs!!) (fst $ unzip rs2)
  




