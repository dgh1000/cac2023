
import qualified Sound.PortMidi as PM
import qualified Data.ByteString as B
import Control.Concurrent
import Control.Monad
import Control.Monad.State
import System.Random
import System.Random.MWC
import System.CPUTime
import Sound.PortMidi hiding (name,initialize)
import Text.Printf
import Data.Map(Map)
import Data.Maybe
import Data.Serialize
import Cac
import Cac.Pcs
import Cac.Types
import Cac.Search
import Util.Showable
import Util.Exception
import Midi.MidiData
import Midi.Interface


top = "/Users/Mike/Dropbox/stack/cac/data/"


mkShorts :: Double -> Int -> Int -> Int -> Int -> Int -> [Short]
mkShorts dur stream chan vel t pit = [g 0x90 0, g 0x80 (dur-0.1)]
  where
    g status d = Short (fromIntegral t*dur + d) stream (status+chan-1) pit vel


compToShorts :: Comp1 -> [Short]
compToShorts c = concat $ zipWith (mkShorts 0.3 0 1 80) [0..] (c1Pits c)


loadCache :: IO (Map Pcs (Map Int [Pcs]))
loadCache = do
  b <- B.readFile $ top ++ "0168.pcs"
  case decode b of
    Right x -> return x


runMidi :: Comp1 -> IO ()
runMidi c = do
  mDev <- findNamedDevice "MidiPipe Input 3"
  when (isNothing mDev) (throwMine "MidiPipe Input 3 is not preset")
  mStreams <- startMidi (fromJust mDev) (fromJust mDev+3)
  case mStreams of
    Left err -> putStrLn (show err) >> return ()
    Right streams -> do
      let shs = compToShorts c
      allOff streams
      beginTime <- fromIntegral `liftM` time
      playRawEvents streams (beginTime+200) shs
      threadDelay 1000000
      allOff streams
      stopMidi streams
      return ()
      


search1 :: IO ()
search1 = do
  let motif = pFromList [0,1,5,7]
      subs = analyzeSubsets motif
      pcset_weights = [0,0,0.25,0.5,1]
      rep_weights = [0,2,4,2,0,-1,-2,-3,-4,-5,-6,-7,-8]
      c = Comp1 [60,61] 44 76 15 motif subs 10
        [eval_pcset pcset_weights, eval_rep 8 rep_weights]
        [prune_octRel 8,prune_rep]
      oc = OptConfig 4 0.25 4
  gen <- newStdGen
  cache <- loadCache
  -- let st = ErsState g cache
  cpuT <- getCPUTime
  let result = evalErs (search oc c) gen (State1 cache)
  case result of
    (Left err,log) -> do
      putStrLn "------------------ exception was thrown ---------------- "
      putStrLn $ showExc err
      putStrLn "------------------- log written to log.txt ---------------- "
      writeFile "log.txt" . showiToString $ Component "log" True log
    (Right result,_) -> do
      putStrLn $ showIString result
      t2 <- getCPUTime
      let td = (fromIntegral $ t2-cpuT)/1000000000000 :: Double
      putStrLn $ printf "CPU Time used: %f" td
      -- runMidi result

--------------------------------------------------------------------------------
--                       testing randomness, speed of MWC versus standard

{-

data Foo = Foo

testRand_help :: Ers Foo Double
testRand_help = liftIO randomIO

testRand_help2 :: Ers Foo Double
testRand_help2 = do
  gen <- gets ersGen
  uniform gen

testRand :: IO ()
testRand = do
  result <- withSystemRandom $ asGenIO $ \gen -> evalErs testRand_help2 gen Foo
  case result of
    (Right v,_) -> print v

-}

----------------------------------------------------------------------

main = search1
-- main = testRand

