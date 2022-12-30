{-# LANGUAGE FlexibleContexts,
             TypeSynonymInstances, FlexibleInstances #-}

module Cac.Simpler.Clump where

import qualified Data.Map as M
import System.Random
import Control.Monad.State
import Data.Map(Map)
import Util.RandMonad

data N = N
  { nPitch  :: Int
  , nLoud   :: Double
  , nDur    :: Double
  , nSpan   :: Double
  , nInstr  :: String
  } 

data Motive = Motive
  { mLen      :: Int
  , mSteps    :: [Int]
  , mRegister :: [Double]
  , mSpan     :: [Double]
  , mDyn      :: [Double]
  }

data MotiveDeploy = MotiveDeploy
  { mdTranspose  :: Int
  , mdRetInv     :: (Bool,Bool)
  , mdTempo      :: Double
  }

data C = C
  { cNotes :: Map Int [N]
  }


type AddR s' m = StateT s' m


f :: Double -> StateT Double RMon Double
f x = do
  r1 <- lift $ rRandomR (0,x)
  return r1

type OuterState = Double

type InnerState = StdGen

f2 :: Double -> StateT OuterState RMon Double -> RMon Double
f2 osInit m = do
  genIn <- get
  let -- rOut :: ((Double,OuterState),StdGen)
      ((outerResult,osFinal),genOut) = runRMon (runStateT m 1.0) genIn
1  put genOut
  return 1.0


{-
runExtended :: Monad m => StateT s m a -> s -> m (a,s)
runExtended m sInit = do
  genIn <- get
  let ((outerResult,osFinal),genOut) = runRMon (runStateT m sInit) genIn
  put genOut
  return (outerResult,osFinal)
-}


runExtended2 runIt m sInit = do
  genIn <- get
  let ((outerResult,osFinal),genOut) = runIt (runStateT m sInit) genIn
  put genOut
  return (outerResult,osFinal)

f4 :: Double -> StateT Double RMon ()
f4 d1 = do
  r <- rRandomR (0,0.01)
  d2 <- get
  put $ r*d1*d2


f5 = mapM_ f4 [1,2,3,4]


f6 :: RMon (Double,Double) 
f6 = do
  (_,s1) <- runExtended2 runRMon f5 10
  (_,s2) <- runExtended2 runRMon f5 10
  return (s1,s2)


f7 :: RMon (Double,Double)
f7 = do
  (s1,s2) <- f6
  return (s1,s2)

-- runAddR :: s' is extra state of AddR
--   'i' is initial state
-- runAddR :: AddR s' a -> s' -> RMon (a,s') 
-- runAddR m i = return $ runStateT m i

-- f :: AddR s' a m -> s' -> m (a,s')
-- f m i = runStateT m i

type Rd = StateT Double RMon





instance RandMonad Rd where
  putGen = lift . put
  getGen = lift get

test1 :: Rd Int
test1 = do
  i <- get :: Rd Double
  r <- rRandomR (0,5)
  put $ i+1
  return $ r

test2 :: RMon ()
test2 = do
  s <- get
  let ((x,y),g') = runState (runStateT test1 2) s
  put g'
  

  
  

{-
lkD :: D a -> a
lkD (D _ x) = x

upD :: (a -> a) -> D a -> D a
upD upF (D g x) = D g $ upF x
-}

{-
instance RandMonad (RD a) where
  putGen g = do {D t1 t2 <- get; put (D g t2)}
  getGen   = do {D t1 t2 <- get; return t1}
-}

{-
oneNote :: RD Double ()
oneNote = do
  D g x <- get
  modify (upD (const $ x+1)) 
  

run :: RD [N]
run = do
  let ms = error "foo"  -- motives
      
-}

main = do
  g <- newStdGen
  let (r,_) = runRMon f7 g
  print r
