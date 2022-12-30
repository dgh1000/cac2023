{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Cac.Genetic.Data where

import System.Random
import Control.Monad.State
import Util.Showable
import Util.RandMonad

data Time = Time Int Rational

data GeState = GeState
 { gsNextId  :: Int
 , gsStdGen  :: StdGen }

type Ge = State GeState


instance RandMonad Ge where
  putGen g = modify (\s -> s { gsStdGen = g })
  getGen   = gets gsStdGen

data Note = Note
  { nId       :: Int
  , nTBeg     :: Rational
  , nTEnd     :: Rational
  , nTOffset  :: Maybe Double
  , nPit      :: Int
  , nDyn      :: Double
  , nSounding :: Bool
  }

{-
noteTBegin :: Note -> Double
noteTBegin = fst . nT

noteTEnd = snd . nT
-}


-- always sorted by begin time
data Comp = Comp
  { cNotes  :: [Note] }


-- pitch classes in ascending order but not necessarily starting with 0
data Scale = Scale [Int] 


-- <octave> <scale degree>
data PitScaleDegree = PitScaleDegree Int Int [Int]

data MotiveElem a =
  MeNote
    { mePit  :: a
    , meDur  :: Rational
    , meSpan :: Rational
    }
  | MeRest
    { meSpan :: Rational }

data Motive a = Motive [MotiveElem a]

class MotiveClass a where
  toPitch        :: a -> Int
  showP          :: a -> String


scaleDegreeToPitch :: PitScaleDegree -> Int
scaleDegreeToPitch (PitScaleDegree oct degree scale)
  | degree < length scale = oct*12 + scale !! degree


instance MotiveClass PitScaleDegree where
  toPitch = scaleDegreeToPitch
  showP = const "foo"

data MotiveElemList a = MotiveElemList [MotiveElem a]

