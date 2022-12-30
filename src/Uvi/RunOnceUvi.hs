



module Uvi.RunOnceUvi where


import qualified Data.Map as M
import qualified Control.Exception as E
import qualified Sound.PortMidi as SP
import qualified Data.List as L
import qualified Data.Set as S
import qualified Text.XML.Light as XL
import Debug.Trace
import Sound.PortMidi hiding (name,initialize)
import System.Random
import Control.Exception
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.DeepSeq
import Control.Concurrent
import Text.XML.Light hiding (qName)
import Text.Printf
import Text.Parsec
import Text.Parsec.String
import Data.Map(Map)
import Data.Either
import Data.Maybe
import Data.Set (Set)
import Midi.Interface
import Score.ScoreData
import Score.XmlToScore
import Score.ShowScore
import XmlDoc.ParseXml
import Util.FileUtil
import Util.Exception
import Util.Showable
import Uvi
import Uvi.ToMidi

-- <beg msr> <maybe end msr> <maybe solo meta-instr>
data PlayCmd = PlayCmd Int (Maybe Int) (Maybe String)

-- we want to validate 

runOnce :: UviRunData -> [String] -> IO ()
runOnce rd args = do
   runOnce_2 args rd `catches` handlers



runMidi :: IO (Either PMError [PMStream])
runMidi = do
  mDev <- findNamedDevice "MidiPipe Input 3"
  when (isNothing mDev) (throwMine "MidiPipe Input 3 is not preset")
  startMidi (fromJust mDev) (fromJust mDev+3)



runOnce_2 :: [String] -> UviRunData -> IO ()
runOnce_2 args (UviRunData sinstrs) = do
  let PlayCmd mBeg mEnd mSolo = parseArgs args
  mStreams <- runMidi
  case mStreams of
    Left err -> putStrLn (show err) >> return ()
    Right streams -> do
      score <- readXml
      -- putStrLn "writing score.txt"
      --   >> writeFile "score.txt" (showIString score)
      gen <- newStdGen

      -- proceed after error checking
      let shorts = toMidi score (mBeg,mEnd) 1.0 sinstrs gen
      putStrLn {- "test-patch-sect.txt" -} $ showiToString $ Component "" False shorts
      {-
      shorts2 <- E.evaluate $ force shorts
      allOff streams
      beginTime <- fromIntegral `liftM` time
      playRawEvents streams (beginTime+200) shorts2 `catches` handlers
      threadDelay 1000000
      allOff streams
      stopMidi streams
      return ()
      -}
----------------------------------------------------------------------
--               parsing argument


parseArgs :: [String] -> PlayCmd
parseArgs [s] = case parse playCmd "" s of
  Left err -> throwMine $ "parsing error in argument in excecuting script " ++
                          "that calls RunOnce: " ++ show err
  Right c  -> c
parseArgs _ = throwMine $ "more than one arg in executing script " ++
                          "that calls RunOnce"


playCmd :: Parser PlayCmd
playCmd = do
  begMsr <- many1 digit
  char '-'
  endMsr <- many1 digit
  soloStaff <- between (char '{') (char '}') (many $ noneOf "{}")
  return $ PlayCmd (read begMsr)
                   (case read endMsr of {0 -> Nothing; x -> Just x})
                   (case soloStaff of {"" -> Nothing; s -> Just s})
  


----------------------------------------------------------------------



readXml :: IO Score
readXml = do
  buf <- readFileStrictly "/Users/Mike/out.xml"
  let topElems = onlyElems . parseXML $ buf
  case L.find ((=="score-partwise") . XL.qName . elName) topElems of
    Just e -> return . xmlToScore . parseXScore $ e


handlers = [Handler asyncHandlerM,Handler myHandlerM,Handler ioHandlerM]


myHandlerM :: MyException -> IO ()
myHandlerM (MyException s) = do
  putStrLn $ "Error: " ++ s


ioHandlerM :: IOException -> IO ()
ioHandlerM e = do
  putStrLn (show e)


asyncHandlerM :: AsyncException -> IO ()
asyncHandlerM _ = do
  putStrLn "Interrupted!"
  
