{-

  Variation on Interface.hs that allows printing real-time messages while
  MIDI is being played.

-}

{-



Example of how to use this module. Note: mkRawEvents and playRawEvents
have to be separate calls because we want any exceptions thrown by
mkRawEvents to be caught before forking a new thread to call playRawEvents

do
      rmes <- evaluate $ mkRawEvents MPID.modifierEarlyBy 
              MPID.initialEventsDelay midiEvts 
      let (MidiTime tEnd,_) = last rmes
      putStrLn $ "Last event at " ++ show (fromIntegral tEnd / 1000 :: Float)
      beginTime <- time
      tid <- forkIO $ 
           playRawEvents stream (beginTime + fromIntegral preStartDelay) rmes

where

  preStartDelay: -- Delay caused by moving absolute start time into the past,
                 -- in order to handle midi events with negative start times
    (note July 2012: not sure this is really needed any more)



-}

module Midi.InterfaceWithMessages  where

import qualified Sound.PortMidi as SP
import qualified Data.List as L
import Text.Printf
import System.IO
import Control.Concurrent
import Control.Monad
import Data.Either
import Debug.Trace
import Data.Function
import Data.Bits
import Data.List(sortBy)
import Foreign.C.Types
import Sound.PortMidi hiding (name,initialize)
import Midi.MidiData
import Util.Exception
import Util.Showable
import Midi.Interface


----------------------------------------------------------------------
--   opening/close midi streams

----------------------------------------------------------------------
--          playRawEvents -- stream RawMidiEvent to midi port
--                          in real time


-- playRawEvents
--   Real-time playback of midi data in the input form 
--     [(MidiTime,RawMidiEvent)] 
--      
-- 
playRawEventsMsg :: [PMStream] -> Integer -> [ShortWithMsg] -> IO ()
playRawEventsMsg streams absBegin shorts = do
  let f :: ShortWithMsg -> [(Integer,((Int,Int,Int,Int),Maybe String))]
      f (ShortWithMsg t str status data1 data2 ms) =
        [(round $ 1000*t,((str,status,data1,data2),ms))]
      tuples = L.sortBy (compare `on` fst) $ concatMap f shorts
      lt = case tuples of
        [] -> throwMine "in Interface.hs, no notes"
        xs -> last xs
  putStrLn $ printf "last MIDI event: %.1f secs"
             ((fromIntegral . fst $ lt)/1000::Double)
  playRawEventsMsg' streams absBegin tuples


playRawEventsMsg' _ _ [] = putStrLn "\nDone." >> return ()
playRawEventsMsg' streams absBegin ((t,((streamId,x,y,z),msg)):remain) = do
  spinUntil $ absBegin+t
  when (streamId >= length streams || streamId < 0) 
    (throwMine $ "In playRawEvents', got stream id greater than number " ++
       "of MIDI streams available, OR LESS THAN ZERO")
  let traceMsg (x,y,z) | x == 0xb0 = "yes " ++ show y ++ " " ++ show z
                       | otherwise = show x
  let writeShortTrace (x,y,z) = (traceMsg (x,y,z)) `trace` 
         writeShort (streams !! streamId) $ toPMEvent (x,y,z)
  writeShortTrace (x,y,z)
  case msg of
    Nothing -> return ()
    Just m  -> putStrLn m 
  playRawEventsMsg' streams absBegin remain

configDurTimeClick = 0.001

