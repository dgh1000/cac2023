{-# LANGUAGE FlexibleContexts, TemplateHaskell, MultiParamTypeClasses,
    TypeSynonymInstances, FlexibleInstances, FunctionalDependencies #-}

module Cac.Simpler.Clump where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Maybe
import Sound.PortMidi
import Control.Concurrent
import Text.Printf
import System.Random
import Control.Monad.State
import Data.Map(Map)
import Util.RandMonad
import Cac.Simpler.Rm
import Control.Lens
import Control.Lens.TH
import Util.Showable
import Midi.Interface
import Midi.MidiData
import Util.Exception

instance ShowItemClass N where
  showI (N (tOn,tOff) pit loud instr) =
    SingleLine $ printf "tOn:%6.3f tOff:%6.3f pit:%d" tOn tOff pit

data N = N
  { _nTimes  :: (Double,Double)
  , _nPitch  :: Int
  , _nLoudness   :: Double
  , _nInstr  :: String
  } 

makeFields ''N

data SubMotive = SubMotive
  { smSteps    :: [Int]
  , smInvPoint :: Int
  , smSpan     :: [Double]
  , smDurs     :: [Double]
  , mDyns      :: [Double]
  }


data MotiveNote = MotiveNote
  { _motiveNotePitchRange :: (Int,Int)
  , _motiveNoteSpan     :: Double
  , _motiveNoteDur      :: Double
  , _motiveNoteLoudness :: Double
  , _motiveNoteInstr :: String
  }

makeFields ''MotiveNote

type SubMotiveFunc = [Int] -> Rm MotiveNote
  

smf01 :: SubMotiveFunc
smf01 priorPits = do
  sc <- gets (view sConf) :: Rm SimpleConf
  return $ MotiveNote 60 1 1 1 "foo"


data MotiveDeploy = MotiveDeploy
  { _motiveDeployT       :: Double
  , _motiveDeployTrans   :: Int
  , _motiveDeployRiFlags :: (Bool,Bool)
  , _motiveDeployTempo   :: Double
  , _motiveDeployInstr   :: String
  }

makeFields ''MotiveDeploy
  
{-
data MotiveDeploy = MotiveDeploy
  { mdTranspose  :: Int
  , mdRetInv     :: (Bool,Bool)
  , mdTempo      :: Double
  }
-}

data C = C
  { cNotes :: Map Int [N]
  }


data Rm2_ = Rm2_
  { _r2Deploy  :: SubMotiveDeploy
  , _r2CurrT   :: Double
  }

makeLenses ''Rm2_ 

realizeSubMotive :: SubMotive -> SubMotiveDeploy -> Rm [N]
realizeSubMotive sm smd = do
  fst `liftM` runExtended (allT sm) (Rm2_ smd (smdT smd))


type Rm2 = StateT Rm2_ Rm

allT :: SubMotive -> Rm2 [N]
allT (SubMotive steps mirrorP spans durs dyns) = do
  steps' <- applyTransRetroInvert mirrorP steps
  let z = L.zip4 steps' spans durs dyns
  mapM oneT z :: Rm2 [N]


oneT :: (Int,Double,Double,Double) -> Rm2 N
oneT (step,span,dur,dyn) = do
  midiPit <- stepToMidiPit step
  SubMotiveDeploy _ _ _ tempo _ <- view r2Deploy `liftM` get
  t <- view r2CurrT `liftM` get :: Rm2 Double
  -- baseTempo <- (scBaseTempo . view sConf) `liftM` lift get :: Rm2 Double
  -- SimpleConf scale basePitch baseTempo pitRange
  --   <- view sConf `liftM` lift get :: Rm2 SimpleConf
  modify (set r2CurrT (t+span*tempo))
  return $ N (t,t+tempo*dur) midiPit 5 "piano"


stepToMidiPit :: Int -> Rm2 Int
stepToMidiPit p = do
  s <- lift get
  let SimpleConf scale basePitch _ pitchRange _ _ = s^.sConf
      l = case length scale of
           x | x > 0 -> x
      pIdx = p `mod` l
      pOct = p `div` l
  return $ (scale !! pIdx) + 12*pOct + basePitch


applyTransRetroInvert :: Int -> [Int] -> Rm2 [Int]
applyTransRetroInvert mirrorPitch pitchesIn = do
  s <- get
  let SubMotiveDeploy _ trans (retro,inv) _ _ = view r2Deploy s
      maybeInv | inv       = map (\p -> mirrorPitch-p) pitchesIn
               | otherwise = pitchesIn
      xs = map (+trans) maybeInv
      ys | retro     = reverse xs
         | otherwise = xs
  return ys

  
randomSmd :: Rm SubMotiveDeploy
randomSmd = do
  SimpleConf _ _ _ _ quantum maxT <- gets (view sConf)
  t1 <- rRandomR (0.5,maxT) :: Rm Double
  let t2 = fromIntegral (round (t1/quantum)) * quantum :: Double
  tempo <- (*2) `liftM` rChooseList [0.333,0.666,1.0,1.25,1.333] 
  trans <- rRandomR (-15,15) :: Rm Int
  inv   <- rChooseList [False,True ]
  ret   <- rChooseList [False,True ]
  return $ SubMotiveDeploy t2 trans (ret,inv) tempo "piano" 


doOneSubMotive :: Int -> SubMotive -> Rm [N]
doOneSubMotive reps sm = do
  smds <- replicateM reps randomSmd :: Rm [SubMotiveDeploy]
  concat `liftM` mapM (realizeSubMotive sm) smds


doAllSubMotives :: Int -> [SubMotive] -> Rm [N]
doAllSubMotives reps sms = concat `liftM `mapM (doOneSubMotive reps) sms

data Mn = Mn (Int,Int) (Double,Double) Int Int
  deriving(Show)


mToMn :: N -> Mn
mToMn (N (tBeg,tEnd) pit _ _) = Mn (0,1) (tBeg,tEnd) pit 0x40

toShort :: Mn -> [Short]
toShort (Mn (str,chan) (tBeg,tEnd) pit vel) =
  [ Short tBeg str (0x90+chan-1) pit vel
  , Short tEnd str (0x80+chan-1) pit vel ]


play :: [Mn] -> IO ()
play ms = do
  mDev <- findNamedDevice "MidiPipe Input 3"
  when (isNothing mDev) (throwMine "MidiPipe Input 3 is not preset")
  mStreams <- startMidi (fromJust mDev) (fromJust mDev+3)
  case mStreams of
    Left err -> putStrLn ("boo:" ++ show err) >> return ()
    Right streams -> do
      allOff streams
      threadDelay 300000
      beginTime <- fromIntegral `liftM` time
      let raws = concatMap toShort ms
      playRawEvents streams (beginTime+200) raws
      threadDelay 300000
      allOff streams
      stopMidi streams
      return ()


main = do
  g <- newStdGen
  let c = SimpleConf { _scScale = [0,2,3,5,7,9,11]
                     , _scBasePitch = 60
                     , _scBaseTempo = 1
                     , _scPitchRange = (20,100)
                     , _scSpanQuantum = 0.25
                     , _scMaxDeployedT = 4
                     }
      sm1 = SubMotive [0,1,2,6,5] 2 [1,1,1,1,1] [0.5,2,1,1,1] [4,4,4,4]
      sm2 = SubMotive [0,6,8,-2]  2 [1,1,1,1]   [2,0.5,2,2]   [4,4,4,4]
      -- smd1 = SubMotiveDeploy 2.0 1 (True,False) 60.0 "piano" 
      (e,_) = runRm (doAllSubMotives 2 [sm1,sm2]) (RmState g c)
  case e of
    Left s   -> putStrLn s
    Right ns -> play (map mToMn ns)
  
-- we want to hard code StdGen initialization. we want to save collection of
-- motives. maybe generate 

-- then we start randomly tweaking the deployment. 
