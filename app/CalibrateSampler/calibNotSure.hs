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
  }


test_notes :: [Note]
test_notes = [ Note 1.0 2.0 60 64 ]


makeFields ''Note

noteToShorts :: (Int,Int) -> Note -> [ShortWithMsg] 
noteToShorts (str,chan) (Note tOn tOff pit vel) =
  [ ShortWithMsg (tOn ) str (0x90+chan-1) pit vel (Just $ show vel)
  , ShortWithMsg (tOff) str (0x80+chan-1) pit vel Nothing ]


genNoteVelocityRange :: Double -> Double -> Int -> Double -> Double ->
                        Double -> Int -> Maybe Int -> [Note]
genNoteVelocityRange minVel maxVel nVel t0 span dur pit ks =
    map mkNote2 [0..(nVel-1)] ++ possibleKs
  where
    possibleKs :: [Note]
    possibleKs = case ks of
      Nothing -> []
      Just k  -> [Note (t0-0.1) (t0-0.05) k 64]
    mkNote :: Double -> Double -> Note
    mkNote t vDbl = Note t (t+dur) pit (round vDbl)
    mkNote2 :: Int -> Note
    mkNote2 i = mkNote (computeTime i) (computeVelDbl i)
    computeTime :: Int -> Double
    computeTime i = t0 + (fromIntegral i)*span*2
    computeVelDbl :: Int -> Double
    computeVelDbl i = scale_3_2 0.0 (fromIntegral i)
                                (fromIntegral $ nVel-1) minVel maxVel
    


myInitMidi :: IO [PMStream]
myInitMidi = do
  mDev <- findNamedDevice "MidiPipe Input 3"
  when (isNothing mDev) (throwMine "MidiPipe Input 3 is not preset")
  mStreams <- startMidi (fromJust mDev) (fromJust mDev+3)
  case mStreams of
    Left err -> error ("boo:" ++ show err)
    Right streams -> return streams

{-
            solo flute calibrated against solo violin

  let pitch1 = 72
      pitch2 = 64
      dest1 = (1,3) -- flute
      dest2 = (0,1) -- violin
      minVel1 = 100
      maxVel1 = 127
      vel2  = 127
      span  = 1.2
      dur   = 0.8
      n     = 8
      tBeg1 = 1.0
      tBeg2 = tBeg1 + span
      ks1   = Just (24+2) -- solo flute susLeg
      ks2   = Just (24+3) -- solo violin susLeg
-}

main = do
  --                   CONFIGRATION
  let pitch1 = 60
      pitch2 = 66
      dest1 = (2,1) -- guitar
      dest2 = (0,1) -- violin
      minVel1 = 95
      maxVel1 = 110
      vel2  = 102
      span  = 1.2
      dur   = 0.8
      n     = 6
      tBeg1 = 1.0
      tBeg2 = tBeg1 + span
      ks1   = Nothing -- Just (24+2) -- solo flute susLeg
      ks2   = Just (24+3) -- solo violin susLeg
  --   instrument 1 has varying dynamics, instrument 2 is fixed
  streams <- myInitMidi
  let notes1 = genNoteVelocityRange minVel1 maxVel1 n tBeg1 span dur pitch1 ks1
      notes2 = genNoteVelocityRange vel2    vel2    n tBeg2 span dur pitch2 ks2
      shorts1 = concatMap (noteToShorts dest1) notes1
      shorts2 = concatMap (noteToShorts dest2) notes2
  allOff streams
  -- putStrLn $ showiToString $ Component "" False (map showI notes)
  let dest = (0,1)
  -- mapM_ (putStrLn . show) shorts
  beginTime <- fromIntegral <$> time
  playRawEventsMsg streams beginTime $ shorts1 ++ shorts2
  allOff streams
  stopMidi streams 
  return ()  

