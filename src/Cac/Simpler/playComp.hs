
module Run where

import Control.Monad
import Control.Concurrent
import Data.Maybe
import Sound.PortMidi
import Midi.Interface
import Midi.MidiData
import Util.Exception

play :: [Short] -> IO ()
play raws = do
  mDev <- findNamedDevice "MidiPipe Input 3"
  when (isNothing mDev) (throwMine "MidiPipe Input 3 is not preset")
  mStreams <- startMidi (fromJust mDev) (fromJust mDev+3)
  case mStreams of
    Left err -> putStrLn ("boo:" ++ show err) >> return ()
    Right streams -> do
      allOff streams
      threadDelay 300000
      beginTime <- fromIntegral `liftM` time
      playRawEvents streams (beginTime+200) raws
      
      
