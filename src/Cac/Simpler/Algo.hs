{-# LANGUAGE TupleSections #-}

module Cac.Simpler.Algo where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Util.Array as UA
import Control.Arrow
import Data.Array 
import Data.Set(Set)
import Data.Maybe
import Data.Map.Strict(Map)
import Cac.Simpler.SimplerData
import Util.Math
import Midi.MidiData
import Data.Ratio


data Context = Context
  { cnMotives    :: Map Int [PitchOpt]
  , cnCurrId     :: Int
  , cnCurrPtr    :: Int
  , cnOptHist    :: [PitchOpt]
  , cnElemHist   :: [PitchElem]
  }
  

cnGetCurrMotiveLen :: Context -> Int
cnGetCurrMotiveLen (Context mots currId _ _ _) = length $ cnLookup currId mots


cnLookup k m = case M.lookup k m of {Just x -> x}


----------------------------------------------------------------------


nextPitchOpts :: Context -> [(Int,Int)]
nextPitchOpts ctx@(Context _ mId ptr _ _)
  | ptr >= len = nextPitchOpts' ctx
  | otherwise  = [(mId,ptr+1)]
  where
    len = cnGetCurrMotiveLen ctx


nextPitchOpts' :: Context -> [(Int,Int)]
nextPitchOpts' ctx = map (,0) . M.keys $ cnMotives ctx

  
pitchOptToElems :: Comp -> Int -> PitchOpt -> [Int]
pitchOptToElems comp lastPitch opt
  | isNothing mTess && isNothing mLeap = error "kn23..0"
  | otherwise                          = S.toList . f1 $ f2 fullSet
  where
    PitchOpt mTess mLeap = opt
    fullSet = S.fromList [cAbsMinPit comp..cAbsMaxPit comp]
    f1 pitchSet = case mTess of
      Nothing -> pitchSet
      Just t  -> let (t1,t2) = tessRange comp t
                 in  S.intersection pitchSet $ S.fromList [t1..t2]
    f2 pitchSet = case mLeap of
      Nothing      -> pitchSet
      Just (lo,hi) -> S.intersection pitchSet $ S.fromList
                      [lastPitch+lo..lastPitch+hi]

tessRange :: Comp -> Double -> (Int,Int)
tessRange comp pos = (round sLo,round sHi)
  where
    (pitLo,pitHi) = cTess comp
    tol = cTessTolerance comp
    sHi = scaleClip 0 (pos+tol) 1 pitLo pitHi
    sLo = scaleClip 0 (pos-tol) 1 pitLo pitHi


nextNote :: Comp -> Int -> Note
nextNote comp pitch = Note pitch ts 5 "piano"
  where
    ts = Times (t + 1%3) (t + 2%3)
    t = case M.maxViewWithKey $ cNotes comp of 
      Just ((k,_),_) -> k




search :: Context -> [Comp]
search ctx = map h $ concatMap g opts
  where
    n1 = Note 60 (Times 0 1) 8 "piano"
    notes :: Map Rational [Note]
    notes = M.fromListWith (++) $ map ((tOn . nTimes) &&& (:[])) [n1]
    c :: Comp
    c = Comp notes (48,72) 0.2 30 90 1
    opts :: [(Int,Int)]  
    opts = nextPitchOpts ctx
    g :: (Int,Int) -> [Int]
    g (motId,ptr) = pitchOptToElems c 60 opt
      where
        opt :: PitchOpt
        opt = (!!ptr) . aLookup motId $ cnMotives ctx
    h :: Int -> Comp
    h pitch = compAddNote n c
      where
        n = nextNote c pitch

type TimedMidi = (Double,MidiShort)
        
toRawMidi :: Double -> Double -> Comp -> [TimedMidi]
toRawMidi timeOffset timeScale comp = concatMap g . M.elems $ cNotes comp
  where
    g :: [Note] -> [TimedMidi]
    g ns = concatMap h ns
      where
        h :: Note -> [(Double,MidiShort)]
        h (Note pit (Times tOn tOff) _ _) =
          [ (j tOn ,MidiShort 0 1 0x90 64 64)
          , (j tOff,MidiShort 0 1 0x80 64 64) ]
          where
            j t = timeOffset + timeScale*fromRational t

toOldShort :: (Double,MidiShort) -> Short
toOldShort (t,MidiShort str chan status data1 data2) =
  Short t str (status+chan-1) data1 data2
  

aIndex :: [a] -> Int -> a
aIndex xs idx = case drop idx xs of
  x:_ -> x


aArrLk :: Array Int a -> Int -> a 
aArrLk arr idx | lo <= idx && idx <= hi = arr ! idx
  where
    (lo,hi) = bounds arr
  
    
aLookup k m = case M.lookup k m of {Just x -> x}


arrLkMin :: Array Int a -> a
arrLkMin arr = arr ! (fst $ bounds arr)
 


    
