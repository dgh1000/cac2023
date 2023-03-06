{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}
module Translation.RunOnceTest where

import Translation.RunOnce
import Translation
import qualified Control.Exception as E
import Midi.Interface (findNamedDevice, startMidi, allOff, playRawEvents, stopMidi)
import Control.Monad (when)
import Data.Maybe (isNothing, fromJust)
import Util.Exception (throwMine)
import System.Random
import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Score.ScoreData
import Score.XmlToScore
import Score.XmlToScore_ties
import Text.Printf
import Control.Arrow ((&&&))
import Control.Monad.State
import Control.Monad.Except
import Translation.ToMidi
import Control.DeepSeq
import Control.Concurrent
import Sound.PortMidi
import XmlDoc.XmlDocData
import Util.FileUtil
import Text.XML.Light (onlyElems, parseXML, qName, Element (elName))
import XmlDoc.ParseXml
import Data.Map (Map)
import Common(Loc)
import Util.Showable
import System.Environment (lookupEnv)

runOnce :: RunData -> [String] -> IO ()
runOnce rd args =
  runOnce_a_t args rd `E.catches` handlers2  


runOnce_a_t :: [String] -> RunData -> IO ()
runOnce_a_t args rd =
  case parseArgs args of
    PlayCmd mBeg mEnd mSolo splicePts -> doPlay_t mBeg mEnd mSolo splicePts rd
    SendCtrl stream chan ctrl value -> doSendCtrl stream chan ctrl value
    SendCtrlSet stream setNum -> doSendCtrlSet stream setNum

doPlay_t :: Int -> Maybe Int -> Maybe String -> [String] -> RunData -> IO ()
doPlay_t mBeg mEnd mSolo splicePts (RunData metasIn) = do
  {- mDev <- findNamedDevice "MidiPipe Input 3"
  when (isNothing mDev) (throwMine "MidiPipe Input 3 is not preset")
  mStreams <- startMidi (fromJust mDev) (fromJust mDev+3)
  case mStreams of
    -- Left err -> putStrLn ("boo:" ++ show err) >> return ()
    Left err -> putStrLn ("boo:" ++ show err)
    Right streams ->  do -} 
      -- score :: XScore
      score <- readXmlTest
      let x :: Score
          x = xmlToScore score
      putStrLn "Writing score.txt ... "
      writeFile "score.txt" (showIString x)
      return ()
      {- 
      putStrLn "writing score.txt..."
         >> writeFile "score.txt" (showIString score)
      -}
      {-
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
            runState (runExceptT (toMidi (mBeg,mEnd) splicePts)) s
      -- putStrLn "writing synth.txt..."
      -- writeFile "synth.txt" (concat $ tsDebugOut finalState)
      case err_or_shorts of
        Left msg -> putStrLn msg
        Right (sNotes,shorts) -> do
          -- debugDump finalState
          -- writeSNotes sNotes
          raws <- E.evaluate $ force shorts
          allOff streams
          threadDelay 300000
          beginTime <- fromIntegral `fmap` time
          playRawEvents streams (beginTime+200) raws `E.catches`
                        (handlers streams)
          threadDelay 1000000
          allOff streams
          stopMidi streams
          return ()

-}

readXmlTest :: IO XScore
readXmlTest = do
  env <- lookupEnv "COMPUTER_SYSTEM"
  buf <- case env of
    Just _  -> readFileStrictly "/Users/mike/in.musicxml"
    Nothing -> readFileStrictly "c:\\Users\\micha\\in.musicxml"
  let topElems = onlyElems . parseXML $ buf
  case L.find ((=="score-partwise") . qName . elName) topElems of
    Just e  -> return $ parseXScore e
    Nothing -> error "foo"
      -- let xd = parseXScore e
      -- in return $ xmlToScore xd
