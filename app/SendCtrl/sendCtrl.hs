
import Text.Parsec
import Text.Parsec.String
import System.Environment
import Control.Monad
import Data.Maybe
import Midi.Interface
import System.MIDI
import Util.Exception


-- need several Moog instruments, at least one for each wiring configuration
-- we want to use. pipe 1 is roland, pipe 2 is sibelius sending pitches when
-- input, pipe 3 through 5 

main = do
  dests <- enumerateDestinations
  idx   <- findSystemDevice dests
  conns <- mapM openDestination [dests!!idx,dests!!(idx+1),dests!!(idx+2)]
  mapM_ start conns
  {-
  when (isNothing mDev) (throwMine "Bus 3 is not present")
  eStreams <- startMidi (fromJust mDev) (fromJust mDev+3)
  let streams = case eStreams of
        Left err -> throwMine $ show err
        Right s  -> s
  -}
  [arg] <- getArgs
  let (streamNum,chan,ctrl,value) = case parse streamChanCtrl "" arg of
        Left err -> throwMine $ show err
        Right x  -> x
  --   writeShort (streams !! streamNum) $ toPMEvent (0xB0+chan-1,ctrl,64)
  send (conns!!streamNum) $ MidiMessage chan (CC ctrl value)

{-
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
    Just _  -> findIndexOfDevice "Bus 3" dests
    Nothing -> findIndexOfDevice "port3" dests
  case dev of
    Nothing -> throwMine "Bus 3 or port3 is not present"
    Just d  -> return d

openConnections :: IO [Connection]
openConnections = do
  dests <- enumerateDestinations
  idx <- findSystemDevice dests
  let outputDests = [dests !! idx, dests !! (idx+1), dests !! (idx+2)]
  conns <- mapM openDestination outputDests
  mapM_ start conns
  return conns
-}

streamChanCtrl = do
  ds1 <- many1 digit
  char ':'
  ds2 <- many1 digit
  char ':'
  ds3 <- many1 digit
  char ':'
  ds4 <- many1 digit
  return (read ds1,read ds2,read ds3,read ds4)
