
import Midi.MidiData
import Control.Monad
import Control.Concurrent
import Data.Maybe
import Sound.PortMidi
import Midi.Interface
import Util.Exception
import Util.Math
import Text.Printf
import Text.Parsec
import Text.Parsec.String

data Note = Note Double Double Int Double -- tOn tOff pitch loud

parseDbl :: Parser Double
parseDbl = read <$> (many1 $ oneOf "0123456789.")
  

parseNote :: Parser Note
parseNote = do
  many space
  d1 <- parseDbl
  many space
  d2 <- parseDbl
  many space
  i1 <- read <$> many1 digit
  many space
  d3 <- parseDbl
  return $ Note d1 d2 i1 d3


parseNotes :: Parser [Note]
parseNotes = many parseNote


readNoteFile :: IO [Note]
readNoteFile = do
  buf <- readFile "/Users/Mike/Dropbox/eclipse/MusG/perform.txt"
  case parse parseNotes "" buf of
    Left err -> throwMine $ show err
    Right ns -> return ns

fromNote :: Note -> [Short]
fromNote (Note tOn tOff pit loud) = map g [(tOn,0x90),(tOff,0x80)]
  where
    g (t,status) = Short t 0 (status+1-1) pit
                   (round $ scale 1 loud 8 1 110)

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
      threadDelay 300000
      allOff streams
      stopMidi streams
      return ()


main = do
  notes <- readNoteFile :: IO [Note]
  play $ concatMap fromNote notes
  
