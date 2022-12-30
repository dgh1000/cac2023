
module Comp where

import qualified Data.List as L
import qualified Data.Map as M
import Control.Arrow
import System.Random
import Sound.PortMidi
import Control.Monad
import Control.Monad.State
import Data.Map(Map)
import Data.Maybe
import Midi.MidiData
import Midi.Interface
import Util.Exception
import Cac
import Common


-- where to go from here? what would be the first composition I want to produce?
--
-- possible models
--
--   harmonic patterns, rhythmic patterns, register, average tempo
--
--   what's the simplest useful generator? I'm not sure. any good music will involve several
--   "evaluation functions", it won't be very good before then. More likely need shape
--   functions. shape function.
--
--   we do need some basic means for creating, manipulating, and displaying
--   compositions. including possible editing tools. 


-- hierarchical

-- do notes have a simple dest? or maybe an instrument?:

-- should I have some kind of beat that reflects the structure of the phrase? no actually
-- better to have it be something that can be translated to music notation

-- should notes have timing model that varies?

data Note t = Note
  { nTBeg  :: t
  , nTEnd  :: t
  , nInstr :: String
  , nPit   :: Int
  , nAmp   :: Double  -- in dB: 0 is loudest
  }
  deriving(Show)

data Comp t = Comp
  { cNotes :: Map Double (Note t)
  }


-- generate random notes within Rio

-- where should I go from here?
--
--   generate compositions via search
--
--   what algorithms do I need other than search? is there any other way I plan to generate
--   them?
--
--   utilities that will be needed? like export to Sibelius, import from Sibelius? well it will
--   be music XML
--
--   make Qt browser: we don't know yet if we need that
--
--   to export to MusicXml we need to have a time model: beats of a measure: 

{-
genRandomNote :: Loc -> Rio (Note
genRandomNote (Loc msr beat) = do
  x <- rioChoose [48..72]
  rioChoose [Note (Loc msr beat) (Loc msr $ beat+1) "piano" x 0]

genNRandomNotes :: Int -> Rio [Note]
genNRandomNotes n = do
  let bs = map (\x -> Loc x 0) [0..n-1]
  mapM genRandomNote bs

main5 = do
  gen <- newStdGen
  out <- evalStateT (genNRandomNotes 5) gen
  print out
-}


{-
data MidiShort = MidiShort
  { msTime   :: Double
  , msStream :: Int
  , msChan   :: Int
  , msStatus :: Int
  , msData1  :: Int
  , msData2  :: Int
  }
-}

locToTime :: Loc -> Double
locToTime (Loc msr beat) = 4*(fromIntegral msr-1) + (fromRational beat - 0)

instrToDest _ = (0,1)

ampToVel x = floor $ 127 * 10 ** (x/20)

toMidi :: Note Double -> [Short]
toMidi (Note beg end instr pit amp) = [ Short beg stream (chan+0x90-1) pit vel
                                      , Short end stream (chan+0x80-1) pit vel ]
  where
    (stream,chan) = instrToDest instr
    vel = ampToVel amp


tuplesToNotes :: [(Int,Double,Double)] -> [Note Double]
tuplesToNotes tups = snd h
  where
    h :: (Double,[Note Double])
    h = L.mapAccumL g 0 tups
    g :: Double -> (Int,Double,Double) -> (Double,Note Double)
    g t (pit,tDelta,amp) = (end,Note t end "" pit amp)
      where end = t+tDelta



oneRandomNote :: Rio (Int,Double,Double)
oneRandomNote = do
  p <- rioRandomR (48,72)
  td <- rioRandomR (0.1,0.3)
  a <- rioRandomR (-6,0)
  return (p,td,a)


nRandomNotes :: Int -> Rio [Note Double]
nRandomNotes n = do
  tuples <- replicateM n oneRandomNote
  return $ tuplesToNotes tuples

randomComp :: Int -> Rio (Comp Double)
randomComp n = do
  let f (Note t _ _ _ _) = t
  notes <- nRandomNotes n
  return . Comp . M.fromList $ map (f &&& id) notes


playComp :: Comp Double -> [Short]
playComp (Comp m) = concatMap toMidi $ M.elems m


main :: IO ()
main = do
  dev <- findNamedDevice "MidiPipe Input 3"
  when (isNothing dev) (throwMine "MidiPipe Input 3 is not present")
  eStreams <- startMidi (fromJust dev) (fromJust dev+ 0 )
  case eStreams of
    Left err -> putStrLn $ show err
    Right streams -> do
      gen <- newStdGen
      notes2 <- evalStateT (randomComp 40) gen
      notes <- evalStateT (nRandomNotes 40) gen
      {- let notes = [Note (Loc 1 1) (Loc 3 1) "foo" 60 0] -}
      let shorts = concatMap toMidi notes
      t <- time
      playRawEvents streams (fromIntegral t+200) shorts
      allOff streams
      stopMidi streams
      return ()
{-      
testRio :: Rio Int
testRio = return 3



main3 :: IO ()
main3 = do
  gen <- newStdGen
  out <- runStateT testRio gen
  print out
-}

  
