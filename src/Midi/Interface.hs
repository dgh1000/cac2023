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
-- import qualified Sound.PortMidi as SP
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
-- import Sound.PortMidi hiding (name,initialize)
import System.MIDI
import Midi.MidiData
import Util.Exception
import Util.Showable


----------------------------------------------------------------------
--   opening/close midi streams
{-
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

-}

findIndexOfDevice :: String -> [Destination] -> IO (Maybe Int)
findIndexOfDevice name dests = do
  names <- zip [0..] <$> mapM getName dests
  let rightNames = filter (\(_,n) -> n == name) names
  case rightNames of
    [] -> return Nothing
    (h:_) -> do
      return $ Just $ fst h

findSystemDevice :: [Destination] -> IO Int
findSystemDevice dests = do
  e <- lookupEnv "COMPUTER_SYSTEM"
  dev <- case e of
    Just _  -> findIndexOfDevice "MidiPipe Input 3" dests
    Nothing -> findIndexOfDevice "port3" dests
  case dev of
    Nothing -> throwMine "MidiPipe Input 3 or port3 is not present"
    Just d  -> return d

openConnections :: IO [Connection]
openConnections = do
  dests <- enumerateDestinations
  idx <- findSystemDevice dests
  let outputDests = [dests !! idx, dests !! (idx+1), dests !! (idx+2)]
  conns <- mapM openDestination outputDests
  mapM_ start conns
  return conns

----------------------------------------------------------------------
--           notes off utilities

-- all notes off CC 123 0
allNotesOff :: Connection -> IO ()
allNotesOff conn = do
  let doChan c = send conn $ MidiMessage c (CC 123 0)
  mapM_ doChan [1..16]

pedalOff :: Connection -> IO ()
pedalOff conn = do
  -- let doChan c = writeShort stream $ toPMEvent (0xB0+c-1,0x40,0)
  let doChan c = send conn $ MidiMessage c (CC 0x40 0)
  mapM_ doChan [1..16]

allOff :: [Connection] -> IO ()
allOff streams = mapM_ allNotesOff streams >> mapM_ pedalOff streams

shortToMessage :: Short -> (Double,(Int,MidiMessage))
shortToMessage (Short time connNum status note vel) =
    (time,(connNum,midiMsg))
  where
    chan = fromIntegral $ status .&. 0x0f
    statusType = fromIntegral $ shift status (-4) 
    shortMsg = ShortMessage chan statusType (fromIntegral note) 
      (fromIntegral vel)
    midiMsg = translateShortMessage shortMsg


----------------------------------------------------------------------
--          playRawEvents -- stream RawMidiEvent to midi port
--                          in real time


-- playRawEvents
--   Real-time playback of midi data in the input form 
--     [(MidiTime,RawMidiEvent)] 
--      
playRawEvents :: [Connection] -> Integer -> [Short] -> IO ()
playRawEvents conns absBegin shorts = do
  let f :: Short -> (Integer,(Int,MidiMessage))
      f sh = (round $ 1000*t,msg)
        where
          (t,msg) = shortToMessage sh
      tuples = L.sortBy (compare `on` fst) $ map f shorts
      lt = case tuples of
        [] -> throwMine "in Interface.hs, no notes"
        xs -> last xs
  putStrLn $ printf "last MIDI event: %.1f secs"
             ((fromIntegral . fst $ lt)/1000::Double)
  playRawEvents' conns absBegin tuples


type StreamId = Int
type MidiStatus = Int
type MidiData0 = Int
type MidiData1 = Int
type Evt = (StreamId,MidiStatus,MidiData0,MidiData1)
type TimeStampEvt = (Integer,Evt)

playRawEvents' :: [Connection] -> Integer -> [(Integer,(Int,MidiMessage))] -> IO ()
playRawEvents' _ _ [] = void (putStrLn "\nDone.")
playRawEvents' streams absBegin evts@((t,_):_) = do
  -- let (sameTime,remainTimes) = takeEventsWhile evts
  let (sameTimes,remainEvts) = L.span ((==t) . fst) evts
  spinUntil (head streams) (absBegin+t)
  forM_ sameTimes (\(_,(streamId,msg)) -> do
    when (streamId >= length streams || streamId < 0) 
      (throwMine $ "In playRawEvents', got stream id greater than number " ++
        "of MIDI streams available, OR LESS THAN ZERO")
    -- writeShort (streams !! streamId) $ toPMEvent (x,y,z))
    send (streams !! streamId) msg)
  playRawEvents' streams absBegin remainEvts

configDurTimeClick = 0.001

-- spinUntil: takes number of milliseconds
spinUntil :: Connection -> Integer -> IO ()
spinUntil conn t = do
  threadDelay 50
  c <- currentTime conn
  when (fromIntegral c < t) $ spinUntil conn t


