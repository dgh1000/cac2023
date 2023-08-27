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

module Midi.Interface  where

import System.Environment
import qualified Sound.PortMidi as SP
import qualified Data.List as L
import Text.Printf
import System.IO
import Control.Concurrent
import Control.Monad
import Data.Either
import Debug.Trace
import Data.Maybe
import Data.Function
import Data.Bits
import Data.List(sortBy)
import Foreign.C.Types
import Sound.PortMidi hiding (name,initialize)
import Midi.MidiData
import Util.Exception
import Util.Showable


----------------------------------------------------------------------
--   opening/close midi streams

startMidi :: Int -> Int -> IO (Either PMError [PMStream])
startMidi midiDevNumLow midiDevNumHigh = do
  SP.initialize
  result <- mapM openOutputDevice [midiDevNumLow..midiDevNumHigh]
  let (errors,streams) = partitionEithers result
  case errors of
    [] -> return $ Right streams
    (err:_) -> return $ Left err


stopMidi :: [PMStream] -> IO ()    
stopMidi streams = do
  mapM_ close streams
  terminate
  return ()    


promptForMidi :: IO (Either PMError [PMStream])
promptForMidi = do
  c <- countDevices
  mapM_ showMidiDevice [0..c-1]
  putStr "Enter midi port number or hit enter for Csound:"
  hFlush stdout
  li <- getLine
  case reads li of
    []      -> throwMine "Something wrong in port number."
    (i,_):_ -> startMidi i i


openOutputDevice :: Int -> IO (Either PMError PMStream)
openOutputDevice devNum = do
  result <- openOutput devNum 0
  case result of
    Right stream -> return $ Right stream
    Left err -> ("openOutputDevice err:" ++ show devNum) `trace`
      (return $ Left err)


showMidiDevice :: Int -> IO ()
showMidiDevice x = do
  di <- getDeviceInfo x
  putStrLn $ printf "%d: %s '%s'" x (decideInputOrOutput di) (SP.name di) 


decideInputOrOutput di = if input di then "Input :" else "Output:"


findNamedDevice :: Bool -> String -> IO (Maybe DeviceID)
findNamedDevice enforceOutput name = do
  c <- countDevices
  let test n = do info <- getDeviceInfo n
                  let flag = not enforceOutput || output info
                  return $ SP.name info == name && flag
  tests <- mapM test [0..c-1]
  return $ snd <$> L.find fst (zip tests [0..c-1])
    

findSystemDevice :: IO (Maybe DeviceID)
findSystemDevice = do
  e <- lookupEnv "COMPUTER_SYSTEM"
  dev <- case e of
    Just _  -> findNamedDevice False "MidiPipe Input 3"
    Nothing -> findNamedDevice True "port3"
  when (isNothing dev) (throwMine "MidiPipe Input 3 or port3 is not present")
  return dev

----------------------------------------------------------------------
--           notes off utilities

allNotesOff :: PMStream -> IO ()
allNotesOff stream = do
  let doChan c = writeShort stream $ toPMEvent (0xB0+c-1,123,0)
  mapM_ doChan [1..16]


pedalOff :: PMStream -> IO ()
pedalOff stream = do
  let doChan c = writeShort stream $ toPMEvent (0xB0+c-1,0x40,0)
  mapM_ doChan [1..16]

allOff :: [PMStream] -> IO ()
allOff streams = mapM_ allNotesOff streams >> mapM_ pedalOff streams



----------------------------------------------------------------------
--          playRawEvents -- stream RawMidiEvent to midi port
--                          in real time


-- playRawEvents
--   Real-time playback of midi data in the input form 
--     [(MidiTime,RawMidiEvent)] 
--      
-- 
playRawEvents :: [PMStream] -> Integer -> [Short] -> IO ()
playRawEvents streams absBegin shorts = do
  let f :: Short -> [(Integer,(Int,Int,Int,Int))]
      f (Short t str status data1 data2) =
        [(round $ 1000*t,(str,status,data1,data2))]
      tuples = L.sortBy (compare `on` fst) $ concatMap f shorts
      lt = case tuples of
        [] -> throwMine "in Interface.hs, no notes"
        xs -> last xs
  putStrLn $ printf "last MIDI event: %.1f secs"
             ((fromIntegral . fst $ lt)/1000::Double)
  playRawEvents' streams absBegin tuples

type StreamId = Int
type MidiStatus = Int
type MidiData0 = Int
type MidiData1 = Int
type Evt = (StreamId,MidiStatus,MidiData0,MidiData1)
type TimeStampEvt = (Integer,Evt)

playRawEvents' :: [PMStream] -> Integer -> [TimeStampEvt] -> IO ()
playRawEvents' _ _ [] = void (putStrLn "\nDone.")
playRawEvents' streams absBegin evts@((t,_):_) = do
  -- let (sameTime,remainTimes) = takeEventsWhile evts
  let (sameTimes,remainEvts) = L.span ((==t) . fst) evts
  spinUntil $ absBegin+t
  forM_ sameTimes (\(_,(streamId,x,y,z)) -> do
    when (streamId >= length streams || streamId < 0) 
      (throwMine $ "In playRawEvents', got stream id greater than number " ++
       "of MIDI streams available, OR LESS THAN ZERO")
    writeShort (streams !! streamId) $ toPMEvent (x,y,z))
  playRawEvents' streams absBegin remainEvts

-- takeEventsWhile :: [TimeStampEvt] -> ([Evt],[TimeStampEvt])
-- takeEventsWhile [] = throwMine "empty list in Interface:takeEventsWhile" 
-- takeEventsWhile l@((t,evt):remain) = (map snd same,remain)
--   where
--     (same,remain) = L.span ((==t) . fst) l

configDurTimeClick = 0.001

-- spinUntil: takes number of milliseconds
spinUntil :: Integer -> IO ()
spinUntil t = do
  threadDelay 50
  c <- time
  when (fromIntegral c < t) $ spinUntil t


----------------------------------------------------------------------
--       utility to convert to the actual type used by the PortMidi
--       library, PMEvent

-- toPMEvent
--   Converts RawMidiEvent to PMEvent (note, the latter includes a time,
--   but since we do playback with the PortMidi's function writeShort, 
--   the PortMidi library will ignore the time, so we just set it to zero)
toPMEvent :: (Int,Int,Int) -> PMEvent
toPMEvent (status,data1,data2) = PMEvent msg 0
  where msg = encodeMsg $ PMMsg (fromIntegral status) (fromIntegral data1)
                                (fromIntegral data2)
