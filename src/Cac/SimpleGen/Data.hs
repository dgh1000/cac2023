{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Cac.SimpleGen.Data where

import Control.Monad.State
import System.Random
import Util.RandMonad

-- time
data Note = Note Int Int Int  -- time dur pitch

data Pitch = Pitch Int

data PitC = PitC Int

data Scale = Scale [PitC]

-- some generation: a set of different durations, spans, and pitches,
-- registers, pitch orders

data DurC = DurC Int

data SpanC = SpanC Int

data Comp = Comp [Note]

data Clump a = Clump [a]

data RegisterC = RegisterC Int

{-
type Gr = State StdGen

instance RandMonad Gr where
  putGen = put
  getGen = get
-}
