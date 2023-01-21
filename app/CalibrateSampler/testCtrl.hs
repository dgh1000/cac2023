{-# LANGUAGE TemplateHaskell, TemplateHaskellQuotes, MultiParamTypeClasses,
             FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}

-- probably want to play note at different volume until we match by ear
--
-- a command to play a specific velocity or step by
--
-- command line
--
--  play <ch1> <vel1> <ch2> <vel2> <vel3>

import qualified Data.List as L
import Control.Lens.TH
import Control.Lens
import Control.Monad.State
import Control.Monad
import Control.Exception
import Data.Maybe
import Midi.Interface
import Midi.InterfaceWithMessages
import Midi.MidiData
import Sound.PortMidi
import System.Random
import Text.Printf
import Util.Exception
import Util.RandMonad
import Util.Showable
import Util.Math

data Note = Note
  { _noteTOn  :: Double
  , _noteTOff :: Double
  , _notePit  :: Int
  , _noteVel  :: Int
  , _noteKs   :: Maybe Int
  }
  
makeFields ''Note

data ControlValue = ControlValue
  { _controlValueCtrlNum :: Int
  , _controlValueValue   :: Int
  }

type ControlCurve = [(Double,ControlValue)]

-- test_notes :: [Note]
-- test_notes = [ Note 1.0 2.0 60 64 ]
-- data ShortWithMsg = ShortWithMsg Double Int Int Int Int (Maybe String)

testControlCurve :: ControlCurve
testControlCurve = map f [t0,t0+0.05..t1]
  where
    t0 = 1.0
    t1 = 10.0
    v0 = 15.0
    v1 = 120.0
    f :: Double -> (Double,ControlValue)
    f t = (t,ControlValue 25 v2)
      where
        v = sin $ scale_3_2 t0 t t1 0.0 pi
        v2 = round $ scale_3_2 0.0 v 1.0 v0 v1


controlCurveToShorts :: ControlCurve -> [ShortWithMsg]
controlCurveToShorts = map f
  where
    f :: (Double,ControlValue) -> ShortWithMsg
    f (t,ControlValue ctrlNum value) = ShortWithMsg t 0 0xb0 ctrlNum value Nothing 


    -- data Note = Note
    -- { _noteTOn  :: Double
    -- , _noteTOff :: Double
    -- , _notePit  :: Int
    -- , _noteVel  :: Int
    -- , _noteKs   :: Maybe Int
    -- }
    
testNotes :: [Note]
testNotes = zipWith f [1.0,2.0..] [60,62..74]
  where
    f t p = Note t (t+1.0) p 64 Nothing

noteToShorts :: (Int,Int) -> Note -> [ShortWithMsg] 
noteToShorts (str,chan) (Note tOn tOff pit vel mKs) =
  [ ShortWithMsg (tOn ) str (0x90+chan-1) pit vel (Just $ show vel)
  , ShortWithMsg (tOff) str (0x80+chan-1) pit vel Nothing ] ++ x
  where
    x = case mKs of
      Just k  -> [ShortWithMsg (tOn-0.1) str (0x90+chan-1) k vel Nothing]
      Nothing -> []


myInitMidi :: IO [PMStream]
myInitMidi = do
  mDev <- findSystemDevice
  -- when (isNothing mDev) (throwMine "MidiPipe Input 3 is not preset")
  mStreams <- startMidi (fromJust mDev) (fromJust mDev+3)
  case mStreams of
    Left err -> error ("boo:" ++ show err)
    Right streams -> return streams


asyncHandlerM :: [PMStream] -> AsyncException -> IO ()
asyncHandlerM streams _ = do
  allOff streams
  putStrLn "Interrupted"
  

playNotes :: [ShortWithMsg] -> IO ()
playNotes shorts = do
  streams <- myInitMidi
  allOff streams
  -- putStrLn $ showiToString $ Component "" False (map showI notes)
  -- mapM_ (putStrLn . show) shorts
  beginTime <- fromIntegral <$> time
  playRawEventsMsg streams beginTime shorts `catches` handlers streams
  allOff streams
  stopMidi streams 

handlers streams = [ Handler (asyncHandlerM streams)]


main = do
  --                   CONFIGRATION
  let -- timing and duration
      ns = testNotes
      nss = concatMap (noteToShorts (0,1)) ns 
      cc = controlCurveToShorts testControlCurve
       
  playNotes (nss ++ cc)
  return ()  

