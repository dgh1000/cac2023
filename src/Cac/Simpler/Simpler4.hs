module Cac.Simpler.Simpler4 where

import qualified Data.Map as M
import qualified Data.List as L
import Sound.PortMidi
import Control.Concurrent
import Data.Maybe
import Debug.Trace
import Data.Map.Strict(Map)
import Data.Function
import Control.Monad
import Control.Monad.State
import Codec.Midi
import Util.RandMonad
import System.Random
import Util.Math
import Util.Exception
import Midi.Interface
import Midi.MidiData

-- evaluation function
--
--   features that jump out... or things that repeat... make sure they form a
--   good shape
--
--   good shape would be
--
--     moving up smoothly
--
--     moving down smoothly
--
--     balanced distribution
--
--     alternating steps and leaps, balanced distribution of step up, step
--     down, leap up, leap down
--
--   good harmonies
--
--     could implement simple machine learning: features of harmony would be
--     root note and pitch class set... that might be all we need
--

-- AUGUST 1, 2018: commenting out to help all of 'cac' build

{-

data N = N
  { nPc :: Int
  , nRange :: Double
  , nLoud :: Double
  , nDur :: Double
  , nSpan :: Double
  , nInstr :: String
  } 


data Conf = Conf
  { cPitches :: [Int]
  , cRanges  :: [Double]
  , cDurs    :: [Double]
  , cSpans   :: [Double]
  , cLouds   :: [Double]
  }
   

data SimpleConf = SimpleConf
  { scN            :: Int
  , scTranspose    :: Int
  , scRangesLo     :: Double
  , scRangesHi     :: Double
  , scDursLo       :: Double
  , scDursHi       :: Double
  , scSpansLo      :: Double
  , scSpansHi      :: Double
  , scLoudsLo      :: Double
  , scLoudsHi      :: Double
  }

data Mn = Mn (Int,Int) (Double,Double) Int Int
  deriving(Show)

seriesMinMaj :: [Int]
seriesMinMaj = [0, 2, 3, 5, 7, 9, 11]

seriesMaj :: [Int]
seriesMaj    = [0, 2, 4, 5, 7, 9, 11]

loudSeries :: [Double]
loudSeries   = [2, 3, 4, 5, 6, 7, 8]

durSeries  :: [Double]
durSeries    = [2, 3, 3.5, 4, 4.5, 5, 6]

spanSeries  :: [Double]
spanSeries = map (* 0.25) [2, 3, 3.5, 4, 4.5, 5, 6]

instrSeries :: [String]
instrSeries = ["a", "b", "c", "a", "b", "c", "a"]


octSeries :: [Double]
octSeries = [2.5,3,4,4.5,5,5.5,6]


toMidiFile :: [Mn] -> Midi
toMidiFile ms = error "foo"

{-
track0 = [ (0 , NoteOn 0 60 80)
         , (24, NoteOff 0 60 0)
         , (0 , TrackEnd) ]

m1 = Midi SingleTrack (TicksPerBeat 24) [track0]
-}

instrMap :: Map String (Int,Int)
instrMap = M.fromList [ ("a", (0,1))
                      , ("b", (0,2))
                      , ("c", (0,3)) ]

computePitch :: Int -> Double -> Int
computePitch pc range = pOut
  where
    pr = range*12 :: Double
    p0 = pc + 12 * (floor range - 1)
    p1 = pc + 12 * (floor range)
    p2 = pc + 12 * (ceiling range)
    p3 = pc + 12 * (ceiling range + 1)
    xs2 = [p0,p1,p2,p3]
    xs = map (\p -> (abs $ pr-fromIntegral p,p)) xs2
    pOut = snd $ L.minimumBy (compare `on` fst) xs 


oneNote :: N -> State Double Mn
oneNote (N pc range loud dur span instr) = do
  t1 <- get
  let pit = computePitch pc range
  let vel = round $ scaleClip 2 loud 8 20 90
  put $ t1+span
  return $ Mn (sLookup instr instrMap) (t1,t1+dur) pit vel

allNotes :: Double -> [N] -> ([Mn],Double)
allNotes tBeg ns = runState (mapM oneNote ns) tBeg


makeSeries :: Double -> Double -> Double -> Int -> RMon [N]
makeSeries spanMult durMult rangeAdd tp = do
  let pitchesIn = map (\pc -> (pc+tp) `mod` 12) seriesMinMaj
      rangesIn = map (+rangeAdd) octSeries
      dursIn = map (*durMult) durSeries
      spansIn = map (*spanMult) spanSeries
  pitches <- rReorderList pitchesIn
  ranges  <- rReorderList rangesIn
  louds   <- rReorderList loudSeries
  durs    <- rReorderList dursIn
  spans   <- rReorderList spansIn
  instrs   <- rReorderList instrSeries
  return $ L.zipWith6 N pitches ranges louds durs spans instrs


makeSeries2 :: Conf -> RMon [N]
makeSeries2 (Conf pitchesIn rangesIn dursIn spansIn loudsIn) = do
  pitches <- rReorderList pitchesIn
  ranges  <- rReorderList rangesIn
  louds   <- rReorderList loudsIn
  durs    <- rReorderList dursIn
  spans   <- rReorderList spansIn
  instrs  <- rReorderList instrSeries
  return $ L.zipWith6 N pitches ranges louds durs spans instrs


{-
twoSeries :: Int -> RMon [N]
twoSeries tp = do
  let ss = map (\pc -> (pc+tp) `mod` 12) seriesMinMaj
  s1 <- makeSeries ss
  s2 <- makeSeries ss
  return $ s1 ++ s2
-}

circleFifths :: RMon [N]
circleFifths = do
  let tps = [0,7..]
      rangeAdds = [0,0.13..]
      spanMults = [1,0.97..]
      durMults =  [0.4,0.39..]
      all = take 13 $ L.zip4 spanMults durMults rangeAdds tps
  concat `liftM` mapM (\(s,d,r,t) -> makeSeries s d r t) all
      
  


toShort :: Mn -> [Short]
toShort (Mn (str,chan) (tBeg,tEnd) pit vel) =
  [ Short tBeg str (0x90+chan-1) pit vel
  , Short tEnd str (0x80+chan-1) pit vel ]


sLookup k m = case M.lookup k m of {Just x -> x}

testRand :: RMon Double
testRand = rRandomR (0,1)

testReorder :: RMon [Int]
testReorder = do
  rReorderList [0..7]

clumpFromSeed :: (Int,Conf) -> [N]
clumpFromSeed (seed,conf) = evalState (makeSeries2 conf) (mkStdGen seed)



 
play :: [Mn] -> IO ()
play ms = do
  mDev <- findNamedDevice "MidiPipe Input 3"
  when (isNothing mDev) (throwMine "MidiPipe Input 3 is not preset")
  mStreams <- startMidi (fromJust mDev) (fromJust mDev+3)
  case mStreams of
    Left err -> putStrLn ("boo:" ++ show err) >> return ()
    Right streams -> do
      allOff streams
      threadDelay 300000
      beginTime <- fromIntegral `liftM` time
      let raws = concatMap toShort ms
      playRawEvents streams (beginTime+200) raws
      threadDelay 300000
      allOff streams
      stopMidi streams
      return ()



main = do
  let cs = concatMap clumpFromSeed [data11,data12,data13]
      (mns,_) = allNotes 1.0 cs
  -- mapM_ (putStrLn . show) mns
  play mns


----------------------------------------------------------------------

spreadOut :: Double -> Double -> Int -> [Double]
spreadOut lo hi n = map (\i -> scale 0 i (fromIntegral $ n-1) lo hi)
                    $ take n [0.0,1.0..]

sctoc :: SimpleConf -> Conf
sctoc (SimpleConf n tsp rangesLo rangesHi dursLo dursHi
          spansLo spansHi loudsLo loudsHi) =
     Conf pitchesOut
          (spreadOut rangesLo rangesHi n)
          (spreadOut dursLo dursHi n)
          (spreadOut spansLo spansHi n)
          (spreadOut loudsLo loudsHi n)
  where
    pitchesOut = map (\pc -> (pc+tsp) `mod` 12) seriesMinMaj


data11 :: (Int,Conf)
data11 =  (3, sctoc $ SimpleConf { scN          = 7
                                 , scTranspose  = 0
                                 , scRangesLo   = 3.5
                                 , scRangesHi   = 7.5
                                 , scDursLo     = 0.2
                                 , scDursHi     = 1.0
                                 , scSpansLo    = 0.3
                                 , scSpansHi    = 1.0
                                 , scLoudsLo    = 3
                                 , scLoudsHi    = 6
                                 } )
                        
data12 :: (Int,Conf)
data12 =  (4, sctoc $ SimpleConf { scN          = 7
                                 , scTranspose  = 0
                                 , scRangesLo   = 3.5
                                 , scRangesHi   = 7.5
                                 , scDursLo     = 0.2
                                 , scDursHi     = 1.0
                                 , scSpansLo    = 0.3
                                 , scSpansHi    = 0.9
                                 , scLoudsLo    = 3
                                 , scLoudsHi    = 7
                                 } )
       
data13 :: (Int,Conf)
data13 = (11, sctoc $ SimpleConf { scN          = 7
                                 , scTranspose  = 0
                                 , scRangesLo   = 4.0
                                 , scRangesHi   = 8.0
                                 , scDursLo     = 0.2
                                 , scDursHi     = 0.8
                                 , scSpansLo    = 0.4
                                 , scSpansHi    = 1.1
                                 , scLoudsLo    = 3
                                 , scLoudsHi    = 7
                                 } )

main2 = do
  let ps = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11]
      ranges = [3, 4, 5, 6]
      doRange r = do
        let rs = map (\p -> computePitch p r) ps
        print r
        print rs
  mapM_ doRange ranges
      
-}
