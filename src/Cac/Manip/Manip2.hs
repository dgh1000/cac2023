{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses, FunctionalDependencies,
    TypeSynonymInstances, FlexibleInstances #-}

module Cac.Manip where

import qualified Data.List as L
import System.Random
import Control.Lens.TH
import Control.Lens
import Control.Monad.State
import Control.Monad.Except
import Util.RandMonad
import Cac.Manip.Util

data N = N
  { _nTimes    :: (Double,Double)
  , _nPitch    :: Int
  , _nLoudness :: Double
  , _nInstr    :: String
  }

makeFields ''N

data Clump = Clump
  { _clumpNotes :: [N]
  }

makeFields ''Clump

data Scale = Scale
  { _scalePcs :: [Int]
  }

makeFields ''Scale

data RScale = RScale
  { _rScaleScale :: Scale
  , _rScaleTrans :: Int
  , _rScaleInv   :: Bool
  }

makeFields ''RScale

data GNote = GNote
  { _gNoteRegister :: Double
  , _gNoteTimes    :: (Double,Double)
  }

makeFields ''GNote


data Gesture = Gesture
  { _gestureNotes :: [GNote]
  }

makeFields ''Gesture

data Conf = Conf
  { _confX :: Int
  }

makeFields ''Conf

data RmState = RmState
  { _rmStateStdGen :: StdGen
  , _rmStateConf   :: Conf
  }

makeFields ''RmState

instance RandMonad Rm where
  getGen = view stdGen `liftM` get
  putGen g = modify (set stdGen g)

type Rm = ExceptT String (State RmState)

randomScales = [ [0,2,3,5,7,9,11]
               , [0,2,4,5,7,9,11]
               , [0,2,3,5,6,8,9,11]
               ]

-- 10   -14   
--  5    -7     **   **
--  0    0      **   **
--  7     7     **
--  2    14     **   **
--  9    21     **   **
--  4    28     
-- 11    35     **
--  6    42          **
--  1    49          
--  8    56          **
--  3    63     **   **





randRScale :: Rm RScale
randRScale = do
  s     <- Scale `liftM` rChooseList randomScales
  trans <- rRandomR (0,11)
  return $ RScale s trans False


spansToRhythms :: [Double] -> [[Double]]
spansToRhythms spans = map g $ L.permutations spans
  where
    g = scanl (+) 0



{-
chooseM' :: Int -> [a] -> [[a]]
chooseM' 0 _ = []
chooseM' n xs = 
-}

chooseRegisters :: Int -> [Double] -> [[Double]]
chooseRegisters n rs = L.nub $ map (take n) $ L.permutations rs
  where
    
    
randGesture7 :: Rm Gesture
randGesture7 = do
  let registers = take 14 [36,39..] :: [Double]
      clumps3   = [ [0,1,1.5]
                  , [0,0.5,1]
                  , [0,0.5,1.5]
                  , [0,1.5,2] ]
      clumps2   = [ [0,0.5]
                  , [0,1]
                  , [0,1.5] ]
      interclump = [2,2.5,3]
  error "foo"
  



test = print $ chooseRegisters 3 [30,40,50,60,70]

data RmData2 = RmData2
  { _rmData2Scale       :: [Int]
  , _rmData2PcSupply    :: [Int]
  }

type Rm2 = StateT RmData2 Rm

instance RandMonad Rm2 where
  getGen = lift getGen
  putGen = lift . putGen

makeFields ''RmData2



fillGesture :: Gesture -> Rm2 [N]
fillGesture (Gesture gNotes) = do
  let l = length gNotes
  pcs  <- gets $ view pcSupply
  pcs2 <- take l `liftM` rPermuteList pcs
  let g pc (GNote reg times) = N times (fromPcRegister pc reg) 4.0 "piano"
  return $ zipWith g pcs2 gNotes


getPcs :: Int -> Rm2 [Int]
getPcs n = error "foo"

getIcSpan :: Rm2 Double
getIcSpan = error "foo"

{-
data GestureConf = GestureConf
  { _gestureConfN         :: Int
  , _gestureConfTimes     :: [Double]
  , _gestureConfRegisters :: [Double]
  , _gestureConfPitches
  }
-}



doGesture :: Double -> [Double] -> [Double] -> [Int] -> Rm ([N],Double)
doGesture tIn spans regs pcs |
  length spans == length regs && length regs == length pcs &&
  length spans > 0 =
    doGesture' tIn spans regs pcs

  
doGesture' :: Double -> [Double] -> [Double] -> [Int] -> Rm ([N],Double)
doGesture' tIn spans regs pcs = do
  let times = scanl (+) 0 spans :: [Double]
      g t reg pc = N (t+tIn,t+tIn+1) (fromPcRegister pc reg) 4.0 "piano"
  return (zipWith3 g times regs pcs,last times)
  

data GestureConf = GestureConf
  { _gestureConfPcs   :: [Int]
  , _gestureConfSpans :: [[Double]]
  , _gestureConfRegs  :: [[Double]]
  }

makeFields ''GestureConf


data SubGestureConf = SubGestureConf
  { _subGestureConfSpanSource     :: [Double]
  , _subGestureConfRegisterSource :: [Double]
  }

buildSubGesture :: RandMonad r => SubGestureConf -> r 
buildSubGesture 

-- what is the issue? we want to generate a "phrase" which consists of several
-- "gestures". each gesture will be made by combining a "span gesture" and a
-- "register gesture". these are called subgestures. there will be a pool of
-- unique span gestures and register gestures. a phrase will be made by
-- pulling one of each type of subgesture out of the pool.
--
-- we need to implement this "pool" idea. 

severalGestures :: GestureConf -> Rm [n]
severalGestures conf = do
  -- we need to put spans, regs in random order. pair with pitches
  error "foo"
  

doIt :: Rm [N]
doIt = do
  let spans1 = [0.25,0.3333,0.5,0.6667]
  rhythm1 <- rPermuteList $ spansToRhythms spans1 :: Rm [[Double]]
  let registers1 = [30,40,50,60,70,80]
      rs = L.permutations
  error "foo"
