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

-- why ks? make sure we're using right patch. no dest specified here
-- dest is given in noteToShorts
data Note = Note
  { _noteTOn  :: Double
  , _noteTOff :: Double
  , _notePit  :: Int
  , _noteVel  :: Int
  , _noteExpr :: Int
  , _noteKs   :: Maybe Int
  }

makeFields ''Note

-- looks like we vary either velocity or expression but not both
--
-- match mf of violin: do 
--
-- problem: find range of notes that match
data TestConfig = 
  TcVaryVel

  { tcFixedExpr :: Int
  , tcVelRange  :: (Int,Int)
  , tcNSteps    :: Int
  } 

  | TcVaryExpr

  { tcFixedVel :: Int
  , tcExprRange  :: (Int,Int)
  , tcNSteps    :: Int
  }
  
  
  
  

-- test_notes :: [Note]
-- test_notes = [ Note 1.0 2.0 60 64 ]

genNotesRange :: TestConfig -> Timing -> Int -> Maybe Int -> [Note]
genNotesRange tc (Timing span dur tBeg) pit mKs =
    map (oneNote . idxToTime) [0..tcNSteps tc-1]
  where
    n = tcNSteps tc
    idxToTime :: Int -> (Double,Int)
    idxToTime idx = (tBeg + fromIntegral idx * span,idx)
    oneNote :: (Double,Int) -> Note
    oneNote (t,idx) = case tc of
      
      TcVaryVel expr (v1,v2) _ ->
        
          let
            
            v = scale_3_2 0 (fromIntegral idx) (fromIntegral n-1)
                            (fromIntegral v1)  (fromIntegral v2)
                      
          in
            Note t (t+dur) pit (round v) expr mKs
          
      TcVaryExpr vel (e1,e2) _ ->
        
          let e = scale_3_2 0 (fromIntegral idx) (fromIntegral n-1)
                    (fromIntegral e1) (fromIntegral e2)
          in Note t (t+dur) pit vel (round e) mKs



noteToShorts :: (Int,Int) -> Note -> [ShortWithMsg] 
noteToShorts (str,chan) (Note tOn tOff pit vel expr mKs) =
  
  [ ShortWithMsg (tOn ) str (0x90+chan-1) pit vel (Just $ show vel)
  , ShortWithMsg (tOff) str (0x80+chan-1) pit vel Nothing
  , ShortWithMsg (tOn-0.1) str (0xB0+chan-1) 11 expr Nothing ]

  ++ x
  
  where
    x = case mKs of
      Just k  -> [ShortWithMsg (tOn-0.1) str (0x90+chan-1) k vel Nothing]
      Nothing -> []

{-
genNoteVelocityRange :: Double -> Double -> Int -> Double -> Double ->
                        Double -> Int -> Maybe Int -> [Note]
genNoteVelocityRange minVel maxVel nVel t0 span dur pit mKs =
    map mkNote2 [0..(nVel-1)]
  where
    mkNote :: Double -> Double -> Note
    mkNote t vDbl = Note t (t+dur) pit (round vDbl) mKs
    mkNote2 :: Int -> Note
    mkNote2 i = mkNote (computeTime i) (computeVelDbl i)
    computeTime :: Int -> Double
    computeTime i = t0 + (fromIntegral i)*span*2
    computeVelDbl :: Int -> Double
    computeVelDbl i = scale_3_2 0.0 (fromIntegral i)
                                (fromIntegral $ nVel-1) minVel maxVel

-}


myInitMidi :: IO [PMStream]
myInitMidi = do
  mDev <- findNamedDevice "MidiPipe Input 3"
  when (isNothing mDev) (throwMine "MidiPipe Input 3 is not preset")
  mStreams <- startMidi (fromJust mDev) (fromJust mDev+2)
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

data Timing = Timing
  { _timingSpan :: Double
  , _timingDur  :: Double
  , _timingTBeg :: Double }

main = do
  let span  = 3.0
      dur   = 1.25
      n     = 4
      tBeg1 = 1.0

      -- Set up test note: the one we are trying to calibrate
      -- Here, flute.
      
      tTest :: TestConfig
      tTest = TcVaryVel 127 (50,70) n
      ksTest = Nothing
      destTest = (1,3) -- flue, keyswitch
      timeTest = Timing span dur (tBeg1+span/2)
      pitTest = 72

      --             Set up
      --        ->  fixed note <-
      --       the one that won't change
      --
      -- Here, violin note 64
      
      fixedVel = 64
      fixedExpr = 64
      fixedPitch = 64
      fixedKs = Just (24+3) -- susLeg
      tFixed :: TestConfig
      tFixed = TcVaryExpr fixedVel (20,90) n
      destFixed = (0,1)
      timeFixed = Timing span dur tBeg1
      pitFixed = 64
      
      notes1 :: [Note]
      notes1 = genNotesRange tTest timeTest pitTest ksTest
      notes2 = genNotesRange tFixed timeFixed pitFixed fixedKs

      shorts1 = concatMap (noteToShorts destTest) notes1
      shorts2 = concatMap (noteToShorts destFixed) notes2
      
  playNotes (shorts1 ++ shorts2)
  return ()  


{-
main2 = do
  --                   CONFIGRATION
  let -- test note: flute susLeg
      dest1 = (2,1) -- guitar
      pitch1 = 67
      ks1 = Nothing

      -- control note: violin susLeg
      dest2 = (0,1) -- violin
      pitch2 = 60
      ks2 = Just 27 -- susLeg

      -- velocity of control note:
      vel2  = 40

      -- velocity of test: 8 steps from 4 to 20
      minVel1 = 41
      maxVel1 = 51

      -- timing and duration
      span = 2.0
      dur   = 1.5
      n     = 5
      tBeg1 = 1.0
      tBeg2 = tBeg1 + span
  let notes1 =
        genNoteVelocityRange minVel1 maxVel1 n tBeg2 span dur pitch1 ks1
      notes2 =
        genNoteVelocityRange vel2    vel2    n tBeg1 span dur pitch2 ks2
      shorts1 = concatMap (noteToShorts dest1) notes1
      shorts2 = concatMap (noteToShorts dest2) notes2
  playNotes (shorts1 ++ shorts2)
  return ()  
-}

