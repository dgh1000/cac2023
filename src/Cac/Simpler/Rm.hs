{-# LANGUAGE TypeSynonymInstances, FlexibleInstances, TemplateHaskell,
    FunctionalDependencies, MultiParamTypeClasses #-}

module Cac.Simpler.Rm where

import Control.Monad.Except
import System.Random
import Control.Monad.State
import Util.RandMonad
import Control.Lens
import Control.Lens.TH


data SimpleConf = SimpleConf
  { _simpleConfScale :: [Int]
  , _simpleConfBasePitch      :: Int
  , _simpleConfBaseTempo      :: Double
  , _simpleConfPitchRange     :: (Int,Int)
  , _simpleConfSpanQuantum    :: Double
  , _simpleConfMaxDeployedT   :: Double
  }

makeFields ''SimpleConf

class MyHasConf a where
  conf :: a -> SimpleConf


instance MyHasConf RmState where
  conf (RmState _ c) = c


type Rm = ExceptT String (State RmState)

data RmState = RmState
  { _rmStateStdGen :: StdGen
  , _rmStateConf   :: SimpleConf
  }

makeFields ''RmState

{-
makeLenses ''RmState
makeLenses ''SimpleConf 
-}

foo :: StdGen -> RmState -> RmState
foo gIn s = s&.stdGen = gIn


runRm :: Rm a -> RmState -> (Either String a,RmState)
runRm m g = runState (runExceptT m) g

instance RandMonad Rm where
  getGen = do {s <- get; return $ _sStdGen s}
  putGen g = modify (set sStdGen g)


runExtended :: StateT s Rm a -> s -> Rm (a,s)
runExtended m sInit = do
  genIn <- get
  let (e,genOut) = runRm (runStateT m sInit) genIn
  -- let ((outerResult,osFinal),genOut) = runRm (runStateT m sInit) genIn
  put genOut
  case e of
    Left err -> throwError err
    Right v  -> return v
 


instance RandMonad (StateT s Rm) where
  getGen = lift getGen
  putGen = lift . putGen

{-
zeroTo5 :: Rm Double
zeroTo5 = do
  r1 <- rRandomR (0,5) :: Rm Double
  return r1

someNote :: Char -> StateT Double Rm (Char,Double)
someNote c = do
  t <- get
  r <- rRandomR (0.9,1.1)
  put $ t+r
  return (c,t)

testRm :: Rm [(Char,Double)]
testRm = do
  r1 <- zeroTo5
  let initialTime = 2.0
  (ns,_) <- runExtended (mapM someNote ['a','b','c']) initialTime
  r2 <- zeroTo5
  return ns

main = do
  gen <- newStdGen
  print $ runRm testRm gen
-}
  
