{-# LANGUAGE TemplateHaskell, TemplateHaskellQuotes, MultiParamTypeClasses,
             FlexibleContexts, FlexibleInstances, FunctionalDependencies #-}

import qualified Data.List as L
import Control.Lens.TH
import Control.Lens
import Control.Monad.State
import Control.Monad
import Data.Maybe
import Midi.Interface
import Midi.MidiData
import Sound.PortMidi
import System.Random
import Text.Printf
import Util.Exception
import Util.RandMonad
import Util.Showable



data Note = Note
  { _noteTOn  :: Double
  , _noteTOff :: Double
  , _notePit  :: Int
  , _noteVel  :: Int
  }


makeFields ''Note


instance ShowItemClass Note where
  showI (Note tOn tOff pit vel) =
    SingleLine $ printf "t: %8.3f %8.3f %2d %2d" tOn tOff pit vel
  

-- Pattern <root> <interval>
data Pattern = Pattern Int [(Int,Int)] 


data Artic = Staccato Double | Legato


data MelodyState = MelodyState
  { _melodyStatePattern   :: Pattern
  , _melodyStateArtic     :: Artic
  , _melodyStateIdx       :: Int
  }

makeFields ''MelodyState


-- <duration> <velocity> <artic> <pitch>
data PatNote = PatNote Double Int Artic Int

----------------------------------------------------------------------
----------------------------------------------------------------------
--                    Sms monad for generating notes


initState stdGen tempo = Ms [] (-1) 0.0 [] stdGen tempo


data Ms = Ms
  { _msCurrPat     :: [PatNote]
  , _msCurrIdx     :: Int
  , _msTNext       :: Double
  , _msAccum       :: [Note]
  , _msGen         :: StdGen
  , _msTempo       :: Double  -- roughly beats per minute
  , _msArtChoices  :: [Artic]
  , _msTessChoices :: (Int,Int)
  , _msDurChoices  :: [Double]
  , _msIntvChoices :: [Int]
  }

makeFields ''Ms


type Sms = State Ms

instance RandMonad Sms where
  putGen g = modify (set gen g)
  getGen = view gen `liftM` get
  

nextNote :: Sms ()
nextNote = do
  st <- get :: Sms Ms
  let pat = view currPat st
      idx = view currIdx st
  if idx < 0 || idx >= length pat 
    then nextNoteNewPat >> nextNote2
    else nextNote2


nextNotes :: Int -> Sms ()
nextNotes n = replicateM n nextNote >> return ()


-- nextNote1 : case of new pattern needed
nextNoteNewPat :: Sms ()
nextNoteNewPat = do
  ps <- nextPatPitches
  durList <- view durChoices <$> get
  dur <- rChooseList durList
  vel <- rChooseList [50,70,90,110::Int]
  -- figure out pitch level
  tempo <- view tempo <$> get
  art <- rChooseList [Staccato 0.1, Legato, Legato]
  let newPat = L.zipWith4 PatNote (repeat (60*dur/tempo)) (repeat vel)
               (repeat art) ps
  -- figure out scale or leap
  -- figure out number
  -- figure out dur
  modify (set currPat newPat)
  modify (set currIdx 0)


nextPatPitches :: Sms [Int]
nextPatPitches = do
  let cs = [0,2,4,5,7,9,11]
  testCh <- view tessChoices <$> get
  pCent  <- rRandomR testCh
  pRange <- rRandomR (4::Int, 5)
  intvCh <- view intvChoices <$> get
  intv <- rChooseList intvCh
  -- come up with list of diatonic pitch numbers
  let ps = [pCent-pRange,pCent-pRange+intv..pCent+pRange]
      doScalePitch p = 12 * (p `div` 7) + cs !! ( p `mod` 7)
      psChrom = map doScalePitch ps
  upDown <- rChooseList [True,False]
  if upDown
    then return psChrom
    else return $ L.reverse psChrom


-- nextNote2 : no new pattern: just find next note
--
nextNote2 :: Sms ()
nextNote2 = do
  idx <- view currIdx <$> get
  pat <- view currPat <$> get
  t   <- view tNext <$> get
  let PatNote dur vel artic pit = lk pat idx
      end = case artic of
        Staccato x -> t + x
        Legato     -> t + dur + 0.1
      n = Note t end pit vel
  modify (\s -> (over accum (++ [n]) .
                 over currIdx (+1) . 
                 over tNext (+dur)) s)



makeNotes :: Int -> Ms -> IO [Note]
makeNotes n st = do
  let (_,stOut) = runState (nextNotes n) st
  return $ view accum stOut
  

lk :: [a] -> Int -> a
lk xs idx | idx >= 0 && idx < length xs = xs !! idx
          | otherwise = throwMine (show idx ++ " m0g87341")


--                    end Sms monad
----------------------------------------------------------------------
----------------------------------------------------------------------

  
myInitMidi :: IO [PMStream]
myInitMidi = do
  mDev <- findNamedDevice "MidiPipe Input 3"
  when (isNothing mDev) (throwMine "MidiPipe Input 3 is not preset")
  mStreams <- startMidi (fromJust mDev) (fromJust mDev+3)
  case mStreams of
    Left err -> error ("boo:" ++ show err)
    Right streams -> return streams


noteToShorts :: (Int,Int) -> Note -> [Short] 
noteToShorts (str,chan) (Note tOn tOff pit vel) =
  [ Short (tOn ) str (0x90+chan-1) pit vel
  , Short (tOff) str (0x80+chan-1) pit vel ]


main = do
  stdGen <- newStdGen

  -- config
  let nNotes    = 80
  let initState = Ms { _msCurrPat = []
                     , _msCurrIdx = -1
                     , _msTNext   = 0.0
                     , _msAccum   = []
                     , _msGen     = stdGen
                     , _msTempo   = 50.0
                     , _msArtChoices = [Legato, Legato, Staccato 0.1]
                     , _msTessChoices = (28,42)
                     , _msDurChoices  = [0.333, 0.5, 0.666]
                     , _msIntvChoices = [1,1,2]
                     }
  
  streams <- myInitMidi
  notes <- makeNotes nNotes initState :: IO [Note]
  allOff streams
  -- putStrLn $ showiToString $ Component "" False (map showI notes)
  let dest = (0,1)
      shorts = concatMap (noteToShorts dest) notes
  mapM_ (putStrLn . show) shorts
  beginTime <- fromIntegral <$> time
  playRawEvents streams beginTime (concatMap (noteToShorts dest) notes)
  allOff streams
  stopMidi streams 
  return ()  




