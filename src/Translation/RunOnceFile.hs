
module Translation.RunOnceFile where

-- The idea: a config file will be a Haskell script that spawns MIDI
-- playback. It will call routines in this module.

import qualified Data.Map.Strict as M
import qualified Control.Exception as E
import qualified Sound.PortMidi as SP
import qualified Data.List as L
import qualified Data.Set as S
import qualified Text.XML.Light as XL
import Debug.Trace
import Sound.PortMidi hiding (name,initialize)
import System.Random
import System.Environment
import Control.Exception
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Control.Monad.Except
import Control.DeepSeq
import Control.Concurrent
import Control.Lens hiding (noneOf)
import Text.XML.Light hiding (qName)
import Text.Printf
import Text.Parsec
import Text.Parsec.String
import Data.Map.Strict(Map)
import Data.Either
import Data.Maybe
import Data.Set (Set)
import Translation
import Translation.ShowTranslation
import Translation.ToMidi
import Midi.Interface
import Score.ScoreData
import Score.XmlToScore
import Score.ShowScore
import XmlDoc.ParseXml
import XmlDoc.ShowXmlDoc
import Util.FileUtil
import Util.Exception
import Util.Showable
import MidiFile.Convert (writeFileMessages)


-- <beg msr> <maybe end msr> <maybe solo meta-instr> <splice points>
-- data PlayCmd = PlayCmd Int (Maybe Int) (Maybe String)

data ArgData = PlayCmd Int (Maybe Int) (Maybe String) [String]
             | SendCtrl Int Int Int Int
             | SendCtrlSet Int Int

-- we want to validate 

runOnceFile :: RunData -> [String] -> IO ()
runOnceFile rd args =
  runOnce_a args rd `catches` handlers2  


runOnce_a :: [String] -> RunData -> IO ()
runOnce_a args rd = do
  case parseArgs args of
    PlayCmd mBeg mEnd mSolo splicePts -> doPlay mBeg mEnd mSolo splicePts rd
    SendCtrl stream chan ctrl value -> doSendCtrl stream chan ctrl value
    SendCtrlSet stream setNum -> doSendCtrlSet stream setNum

{-
MOVED TO interface.hs
findDevice :: IO (Maybe DeviceID)
findDevice = do
  e <- lookupEnv "COMPUTER_SYSTEM"
  dev <- case e of
    Just _  -> findNamedDevice False "MidiPipe Input 3"
    Nothing -> findNamedDevice True "port2"
  when (isNothing dev) (throwMine "MidiPipe Input 3 or port2 is not present")
  return dev
-}

doPlay :: Int -> Maybe Int -> Maybe String -> [String] -> RunData -> IO ()
doPlay mBeg mEnd mSolo splicePts (RunData metasIn) = do
  mDev <- findSystemDevice
  -- when (isNothing mDev) (throwMine "MidiPipe Input 3 or port2 is not present")
  mStreams <- startMidi (fromJust mDev) (fromJust mDev+3)
  case mStreams of
    Left err -> putStrLn ("boo:" ++ show err)
    Right streams -> do
      score <- readXml
      {-
      putStrLn "writing score.txt..."
         >> writeFile "score.txt" (showIString score)
      -}
      gen <- newStdGen
      let scoreStaffNs = M.keysSet $ scStaves score
      -- if a meta is solo, do that now
      let metas = case mSolo of
            Nothing -> metasIn
            Just m  -> case L.find ((==m) . iName) metasIn of
              Nothing -> throwMine $ printf ("you asked to solo meta with" ++
                 "name '%s' but no such meta exists") m
              Just x -> [x]

      -- do some error checking
      let coveredStaffNs = computeCovered metas
      when (coveredStaffNs /= scoreStaffNs)
        (putStrLn $ printf ("WARNING! score staff names are %s; meta " ++
          "coverage is %s") (show scoreStaffNs) (show coveredStaffNs))
      let names = map iName metas :: [String]
      when (length names /= length (S.fromList names))
        (throwMine $ printf "meta names are %s; these are not unique" 
           (show names))

      -- proceed after error checking
      let metaMap = M.fromList $ map (iName &&& id) metas
          s = TrState score metaMap gen M.empty M.empty M.empty [] [] [] []
                      M.empty []
          (err_or_shorts,finalState) =
            runState (runExceptT (toMidiFile (mBeg,mEnd) splicePts)) s
      case err_or_shorts of
        Left msg -> putStrLn msg
        Right (sNotes,fileMessages) -> do
          -- debugDump finalState
          -- writeSNotes sNotes
          raws <- E.evaluate $ force fileMessages
          putStrLn "Writing 'test.mid'"
          writeFileMessages "test.mid" raws
          return ()

----------------------------------------------------------------------
-- debug

writeSNotes :: [SNote] -> IO ()
writeSNotes ss = do
  putStrLn "**** Writing SNotes ****"
  writeFile "snotes.txt" $ showiToString $ Component "" False (map showI ss)



----------------------------------------------------------------------
--               parsing argument



parseArgs :: [String] -> ArgData
parseArgs [s] = case parse inputArg "" s of
  Left err -> throwMine $ "parsing error in argument in excecuting script " ++
                          "that calls RunOnce: " ++ show err
  Right c  -> c
parseArgs _ = throwMine $ "more than one arg in executing script " ++
                          "that calls RunOnce"


inputArg :: ParsecT String () Identity ArgData
inputArg = sendCtrl <|> playCmd <|> sendCtrlSet

sendCtrl :: Parser ArgData
sendCtrl = do
  let r = do ds <- many1 digit
             return (read ds)
  char 'c'
  stream <- r
  char ':'
  chan   <- r
  char ':'
  ctrlNum <- r
  char ':'
  SendCtrl stream chan ctrlNum <$> r

sendCtrlSet :: Parser ArgData
sendCtrlSet = do
  let r = do ds <- many1 digit
             return (read ds)
  char 't'
  stream <- r
  char ':'
  SendCtrlSet stream <$> r


splicePoint = many1 alphaNum

manySplicePoints = sepBy splicePoint (char ',')

playCmd :: Parser ArgData
playCmd = do
  char 'p'
  begMsr <- many1 digit
  char '-'
  endMsr <- many1 digit
  soloStaff    <- between (char '{') (char '}') (many $ noneOf "{}")
  splicePoints <- between (char '{') (char '}')
                  (sepBy splicePoint (char ','))
  return $ PlayCmd (read begMsr)
                   (case read endMsr of {0 -> Nothing; x -> Just x})
                   (case soloStaff of {"" -> Nothing; s -> Just s})
                   splicePoints
  


----------------------------------------------------------------------


computeCovered :: [MetaInstr] -> Set String
computeCovered metas
  | length listing /= length s = throwMine $ printf
      "metas cover some duplicate staves: %s" (show listing)
  | otherwise = s
  where
    listing = concatMap iStaffNs metas
    s = S.fromList listing



debugDump2 :: TrState -> IO ()
debugDump2 ts = do

  -- view timeMods ts :: Map String [UnitTimeMod]
  --
  -- [UnitTimeMod] BECOMES ShowList UnitTimeMod 
  
  let s11 = ShowMap $ M.map ShowList $ view timeMods ts
      s1 = showiToString $ case showI s11 of
             Component _ _ vs -> Component "time mods" True vs
      s2 = showiToString $ Component "notes" True
           (map showI $ concat $ view notesOut ts)
      s3 = showIString $ view score ts
  putStrLn "writing feb.txt..." >> writeFile "feb.txt" s1


debugDump :: TrState -> IO ()
debugDump ts = do
  let s = concat $ view initRaws ts
      c = Component "" False (map showI s)
  {-
  let s1 = showiToString $ Component "time mods" True
                           (map showI $ M.toAscList $ view timeMods ts)
  let s2 = showiToString $ Component "notes" True
           (map showI $ concat $ view notesOut ts)
      s3 = showIString $ view score ts
  -}
  putStrLn "writing initRaws.txt..." >>
    writeFile "initRaws.txt" (showiToString c)


computeMsrRange :: [String] -> (Int,Maybe Int)
computeMsrRange args = case args of
  [beg,"-"] -> (ri beg,Nothing)
  [beg,end] -> (ri beg,Just $ ri end)
  where
    ri :: String -> Int
    ri s = case reads s of { (i,_):_ -> i}

{-
startMidi :: IO (Maybe [PMStream])
startMidi = do
  i <- openMidiPipeInput2
  result <- initMidi i i
  case result of
    Left err -> putStrLn (show err) >> return Nothing
    Right streams -> return $ Just streams


stopMidi :: [PMStream] -> IO ()    
stopMidi streams = do
  mapM_ close streams
  terminate
  return ()    
-}

readXml :: IO Score
readXml = do
  env <- lookupEnv "COMPUTER_SYSTEM"
  buf <- case env of
    Just _  -> readFileStrictly "/Users/mike/in.musicxml"
    Nothing -> readFileStrictly "c:\\Users\\micha\\in.musicxml"
  let topElems = onlyElems . parseXML $ buf
  case L.find ((=="score-partwise") . XL.qName . elName) topElems of
    Just e ->
      let xd = parseXScore e
      in  do putStrLn "Writing xml.txt ... "
             writeFile "xml.txt" $ showIString xd
             return $ xmlToScore xd
    Nothing  -> error "foo"


{-
openMidiPipeInput2 :: IO DeviceID
openMidiPipeInput2 = do
  c <- countDevices
  let test n = do info <- getDeviceInfo n
                  return $ SP.name info == "MidiPipe Input 2"
  tests <- mapM test [0..c-1]
  case L.find fst (zip tests [0..c-1]) of
    Nothing -> throwMine "is MidiPipe running?"
    Just (_,x) -> putStrLn "using MidiPipe Input 2" >> return x


initMidi :: Int -> Int -> IO (Either PMError [PMStream])
initMidi midiDevNumLow midiDevNumHigh = do
  SP.initialize
  
  result <- mapM openOutputDevice [midiDevNumLow..midiDevNumHigh]
  
  let (errors,streams) = partitionEithers result
  case errors of
    [] -> return $ Right streams
    (err:_) -> return $ Left err


openOutputDevice :: Int -> IO (Either PMError PMStream)
openOutputDevice devNum = do
  result <- openOutput devNum 0
  case result of
    Left stream -> return $ Right stream
    Right err -> return $ Left err
-}


doSendCtrl :: Int -> Int -> Int -> Int -> IO ()
doSendCtrl str chan ctrl val = do
  mDev <- findSystemDevice
  -- when (isNothing mDev) (throwMine "MidiPipe Input 3 is not preset")
  mStreams <- startMidi (fromJust mDev) (fromJust mDev+3)
  case mStreams of
    Left err -> putStrLn ("boo:" ++ show err) >> return ()
    Right streams -> do
      let s | str >= length streams = throwMine $ printf ("index %d into " ++
                                       "streams is too high") str
            | str < 0 = error "str < 0"
            | otherwise = streams !! str
      putStrLn $ printf "Writing controller %d, value %d to dest (%d,%d)" ctrl
                 val str chan
      writeShort s $ toPMEvent (0xB0+chan-1,ctrl,val)
      -- writeShort s $ toPMEvent (0x80,0x40,64)
      return ()


----------------------------------------------------------------------

doSendCtrlSet :: Int -> Int -> IO ()
doSendCtrlSet str setNum = do
  let sendOneCtrl theStream ((chan,ctrlNum),value) =
        writeShort theStream $ toPMEvent (0xB0+chan-1,ctrlNum,value)
  mDev <- findSystemDevice
  -- when (isNothing mDev) (throwMine "MidiPipe Input 3 is not preset")
  mStreams <- startMidi (fromJust mDev) (fromJust mDev+3)
  case mStreams of
    Left err -> putStrLn ("boo:" ++ show err)
    Right streams -> do
      let theStr | str >= length streams =
                     throwMine $ printf ("index %d into " ++
                     "streams is too high") str
                 | str < 0   = error "str < 0" 
                 | otherwise = streams !! str
      putStrLn $ printf "Writing control set %d to stream %d" setNum str
      let s | setNum >= 0 && setNum < length controlSets = controlSets !! setNum
      mapM_ (sendOneCtrl theStr) s




----------------------------------------------------------------------

handlers2 = [ Handler myHandlerM
            , Handler ioHandlerM ]

handlers streams = [ Handler (asyncHandlerM streams)
                   , Handler myHandlerM
                   , Handler ioHandlerM]


myHandlerM :: MyException -> IO ()
myHandlerM (MyException s) = do
  putStrLn $ "Error: " ++ s


ioHandlerM :: IOException -> IO ()
ioHandlerM e = do
  putStrLn (show e)


asyncHandlerM :: [PMStream] -> AsyncException -> IO ()
asyncHandlerM streams _ = do
  allOff streams
  putStrLn "Interrupted!"
  

data Adsr = Adsr Int Int Int Int Int

type ControlSet = [((Int,Int),Int)]

controlSet01 :: ControlSet
controlSet01 = [ ((1, 103), 80)  -- filter cutoff
               
               , ((1, 104), 64)  -- filter cutoff mod intensity: linear key
                                 --   follow

               -- filter cutoff ADSR
               , ((1, 105), 64)  -- attack
               , ((1, 106), 64)  -- decay
               , ((1, 107), 64)  -- sustain
               , ((1, 108), 64)  -- release
               , ((1, 114), 64)  -- dynamic
               , ((1, 115), 64)  -- intensity, modulation of cutoff

               -- amp ADSR

               , ((1, 109), 64)  -- attack
               , ((1, 110), 64)  -- decay
               , ((1, 111), 64)  -- sustain
               , ((1, 112), 64)  -- release
               , ((1, 113), 64)  -- dynamic

               
               ]

controlSets :: [ControlSet]
controlSets = [controlSet01]


----------------------------------------------------------------------
--            parse control set file

  
