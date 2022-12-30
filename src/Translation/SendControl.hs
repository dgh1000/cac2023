module Translation.SendControl where

import Text.Parsec
import Text.Printf
import Text.Parsec.String
import Control.Monad
import Sound.PortMidi
import Data.Maybe
import Midi.Interface
import Util.Exception

data OneControl = OneControl Int Int Int
  deriving(Show)


doSendParsedControls :: Int -> IO ()
doSendParsedControls streamNum = do
  streams <- myInitMidi
  ctrls <- parseControlFile
  let sendOneCtrl theStream (OneControl chan ctrlNum ctrlValue) =
        writeShort theStream $ toPMEvent (0xB0+chan-1,ctrlNum,ctrlValue)
      s = streams !! 0
  putStrLn $ printf "Writing control set to stream %d" streamNum
  mapM_ (sendOneCtrl s) ctrls
  allOff streams
  stopMidi streams
  return ()
  

myInitMidi :: IO [PMStream]
myInitMidi = do
  mDev <- findNamedDevice "MidiPipe Input 3"
  when (isNothing mDev) (throwMine "MidiPipe Input 3 is not preset")
  mStreams <- startMidi (fromJust mDev) (fromJust mDev+3)
  case mStreams of
    Left err -> error ("boo:" ++ show err)
    Right streams -> return streams


parseControlFile :: IO [OneControl]
parseControlFile = do
  b <- readFile "/Users/Mike/Dropbox/stack/cset.txt"
  case parse controlFile "" b of
    Left err -> error (show err)
    Right cs -> return cs


controlFile :: Parser [OneControl]
controlFile = do
  many space
  cs <- many oneControl
  many comment
  eof
  return cs

oneControl :: Parser OneControl
oneControl = try $ do
  many comment
  char '('
  many space
  d1 <- many1 digit
  many space
  d2 <- many1 digit
  many space
  d3 <- many1 digit
  many space
  char ')'
  many space
  return $ OneControl (read d1) (read d2) (read d3)



comment :: Parser ()
comment = do
  char '{'
  many $ noneOf "}"
  char '}'
  many space
  return ()
