{-# LANGUAGE ExistentialQuantification, MultiParamTypeClasses,
    FunctionalDependencies #-}

module Cac.Simpler.SimplerData where

import qualified Util.Array as UA
import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Data.Array.IArray
import Data.Map.Strict(Map)
import Data.Ratio


----------------------------------------------------------------------
--                     utility


----------------------------------------------------------------------
--                    composition and note data

data Comp = Comp
  { cNotes          :: Map Rational [Note]
  , cTess           :: (Double,Double)
  , cTessTolerance  :: Double  -- for melody motive elems with a specified
                               -- position in the tessitura, this fraction of
                               -- tessitura that they can deviate from that
                               -- position
  , cAbsMinPit      :: Int
  , cAbsMaxPit      :: Int
  , cBeatDur        :: Double
  }


data Note = Note
  { nPitch  :: Int
  , nTimes  :: Times
  , nDyn    :: Double
  , nTimbre :: String
  }


data Times = Times
  { tOn  :: Rational
  , tOff :: Rational
  }


----------------------------------------------------------------------


-- a motive might indicate a general range, but still allow for
-- variation. repeated some number of times. we need to track concrete
-- choices. probably use separate classes, the usual.
--
-- a motive option and concrete option are two different things
--
-- howabout "option" and "choice" or "element"?

----------------------------------------------------------------------
--                       motives

  
data PitchMotives = PitchMotives
  { pmMotives :: Array Int (Array Int PitchOpt) 
  }


pmLookup :: Int -> PitchMotives -> Array Int PitchOpt
pmLookup idx (PitchMotives mots) = UA.safeIdx "mn32d9" idx mots


pmLookupOpt :: PitchMotives -> Int -> Int -> PitchOpt
pmLookupOpt (PitchMotives motives) motId ptr =
  UA.safeIdx "kn398" ptr $ UA.safeIdx "k3ng99" motId motives 


pmIndices :: PitchMotives -> [Int]
pmIndices = indices . pmMotives


pmMotiveLen :: PitchMotives -> Int -> Int
pmMotiveLen (PitchMotives motives) = error "lengh"

    

data PitchOpt = PitchOpt
  { poTess :: Maybe Double
  , poLeap :: Maybe (Int,Int)
  }


data PitchElem = PitchElem
  { pePitch :: Int
  }


data PitchForm = PfPrime    Int
               | PfRetro    Int
               | PfInv      Int
               | PfRetroInv Int


data PitchHistory = PitchHistory
  { phForm  :: PitchForm
  , phId    :: Int
  , phPtr   :: Int
  , phLen   :: Int 
  , phOpts  :: [PitchOpt]   -- backwards in time
  , phElems :: [PitchElem]  -- backwards in time
  }


phIncrPtr :: PitchOpt -> PitchHistory -> PitchHistory
phIncrPtr opt ph = ph {phPtr = phPtr ph + 1, phOpts = opt : phOpts ph}


phLookupOpt :: Int -> Int -> PitchHistory -> PitchOpt
phLookupOpt = error "foo"


phNew :: Int -> PitchMotives -> PitchHistory
phNew motId motives = PitchHistory PfPrime motId 0 len 


{-
data AnnotOpt o = AnnotOpt
  { moMOpt    :: o
  , ecMotId   :: Int
  , ecPtr     :: Int
  , ecLen     :: Int
  }
-}


{-
updateChoice :: Choice e conc -> conc -> e -> Choice e conc
updateChoice c e = c { ecElem = e
                     , ecHistory = error "foo"
                     , ecPtr = ecPtr c + 1
                     }
-}

{-
class OptionClass motSet o | motSet -> o, o -> motSet where
  nextOpts :: motSet -> AnnotOpt o -> [AnnotOpt o]
-}

----------------------------------------------------------------------
--                     rhythm motives and elements


-- need to store rhythm in progress, last chosen Option and last chosen
-- elem. also chosing tempo is first choice. could be considered first
-- choice. I can't shoehorn. motive in progress.




data RhythmMotives = RhythmMotives
  { rmMotives :: Array Int (Array Int RhythmOpt)
  }


data RhythmOpt = RhythmOpt
  { reFrac :: Rational
  }


data RhythmElem = ReElem
  { reDur  :: Rational
  , reTime :: Rational
  }

data RhythmHistory = RhythmHistory
  { rhBeatDur :: Rational
  , rhId      :: Int
  , rhPtr     :: Int
  , rhOpts    :: RhythmOpt   -- backwards in time
  , rhElems   :: RhythmElem
  }

{-

instance OptionClass RhythmSet RhythmOpt where
  nextOpts (RhythmSet motives) (AnnotOpt o currMotId ptr len)
    | ptr == len = map f $ indices motives
    | otherwise  = [AnnotOpt (sdArrLk currMot $ ptr+1) currMotId (ptr+1) len]
    where
      currMot = motives ! currMotId
      -- g :: String -> Int -> RhythmElem
      -- g name idx = sdIndex (ilElems $ sdLookup name motives) idx
      f motIdx = AnnotOpt (sdArrLk mot 0) motIdx 0 len
         where
           mot = sdArrLk motives motIdx  
-}

----------------------------------------------------------------------



    
sdLookup k m = case M.lookup k m of {Just x -> x} 

sdArrLk :: Array Int a -> Int -> a
sdArrLk arr idx | lo <= idx && idx <= hi = arr ! idx
  where
    (lo,hi) = bounds arr


sdIndex :: [a] -> Int -> a
sdIndex xs idx = case drop idx xs of
  x:_ -> x
