
import Control.Concurrent
import Debug.Trace
import Data.Maybe
import Control.Monad
import Control.Monad.Trans.State
import System.Random
import Sound.PortMidi hiding (name, initialize)
import Midi.Interface
import Midi.MidiData
import Util.Exception
import Cac.Comp

-- what fitness functions will we need?
--
-- spread of notes

main :: IO ()
main = do
  mDev <- findNamedDevice "MidiPipe Input 3"
  when (isNothing mDev) (throwMine "MidiPipe Input 3 is not preset")
  mStreams <- startMidi (fromJust mDev) (fromJust mDev+3)
  case mStreams of
    Left err -> putStrLn ("boo:" ++ show err) >> return ()
    Right streams -> do
      mNotes <- mkRandomData2
      let shorts = let x = concatMap mkOne mNotes
                   in (show $ length x) `trace` x
      allOff streams
      beginTime <- fromIntegral `liftM` time
      playRawEvents streams (beginTime+200) shorts
      threadDelay 1000000
      allOff streams
      stopMidi streams
      return ()


mkOne :: MNote -> [Short]
mkOne (MNote tBeg tEnd chan pit vel) = [g 0x90 tBeg, g 0x80 tEnd]
  where
    g :: Int -> Int -> Short
    g status t = Short (fromIntegral t/1000) 0 (status+chan-1) pit 64


mkRandomComp :: IO Comp
mkRandomComp = error "foo"

type Rm a = State StdGen a

-- rmR :: (Int,Int) -> Rm Int
rmR (x,y) = do
  g <- get
  let (v,g') = randomR (x,y) g
  put g'
  return v


randomCNote :: Int -> Rm (CNote,Int)
randomCNote tBeg = do
  pit <- rmR (48,72)
  loud <- rmR (1,8)
  dur <- rmR (300,1000)
  span <- rmR (300,1000)
  let c = CNote tBeg (tBeg+dur) pit loud
  return (c,tBeg+span)
  
  


mkRandomData2 :: IO [MNote]
mkRandomData2 = do
  gen <- newStdGen
  let n = 10
      oneRandPitch = do
        (v, g) <- randomR (48,72) `liftM` get
        put g
        return v
      ps :: [Int]
      ps = evalState (replicateM n oneRandPitch) gen
      oneMNote :: Int -> Int -> MNote
      oneMNote tBeg pit = MNote tBeg (tBeg+300) 1 pit 64
      ts = [0,500..]
  return $ zipWith oneMNote ts ps
        
