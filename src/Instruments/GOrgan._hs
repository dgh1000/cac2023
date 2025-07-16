
module Instruments.GOrgan where

import qualified Data.Map as M
import Data.Map(Map)
import Instruments
import Instruments.Synth
import Translation


makeChan :: String -> (Int,Int) -> (String,SynthChan)
makeChan chanName dest = (chanName,SynthChan dest M.empty M.empty)


-- makeOrgan
--
--   we need to make a MetaInstr.
--
--     take in
--
--       staff names, map of channel function name to dest, generic shape
--       function
--
--       channel name will probably be organ stop
--
--  set staff names
--
--     make Synth and from
makeOrgan :: [String] -> [(String,(Int,Int))] -> 
             Map String BracketFunc -> GsFunc -> MetaInstr
makeOrgan staffNs chanDestNames bracketFuncs gs =
  makeSynth2 "synth" staffNs mm bracketFuncs gs
    where
      mm = M.fromList $ map (uncurry makeChan) chanDestNames

