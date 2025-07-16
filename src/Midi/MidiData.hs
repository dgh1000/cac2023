{-# LANGUAGE DeriveAnyClass, DeriveGeneric #-}
module Midi.MidiData where

import GHC.Generics hiding(Meta)
import Control.DeepSeq


-- timestamp - in seconds? message type, channel, note, vel
data Short = Short Double Int Int Int Int
             deriving (NFData,Generic,Show)


data FileMessage = 
     FMNoteOn Double Int Int Int
  |  FMNoteOff Double Int Int Int
  |  FMCtrl Double Int Int Int
  deriving (NFData,Generic,Show)

-- Probably don't need this
data ShortWithMsg = ShortWithMsg Double Int Int Int Int (Maybe String)


mkCtrl :: Double -> (Int,Int) -> Int -> Int -> Short
mkCtrl t (str,chan) ctrlNum value = Short t str (0xB0+chan-1) ctrlNum value


