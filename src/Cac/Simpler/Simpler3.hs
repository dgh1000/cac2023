module Cac.Simpler.Simpler3 where

import qualified Data.Map as M
import Data.Map(Map)
import Cac.Simpler.SimplerData
import Codec.Midi 

data Series = Series [Int]

data SeriesPtr = SeriesPtr Int [Int]


class Monad m => MyError m e | m -> e where
  throwMe :: e -> m e
  catchMe :: m e -> 


convert :: MyError m => Map String Instr -> Comp -> m [MidiNote] 
convert instrs comp = error "foo"


