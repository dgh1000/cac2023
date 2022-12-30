module Cac.Manip.Manip where

import Text.Printf
import Codec.Midi


analyzeMidi :: Midi -> String
analyzeMidi (Midi typ timeDiv tracks) =
  printf "%s %s %d" (show typ) (show timeDiv) (length tracks)

main = do
  e <- importFile "pr-synth-02.mid"
  case e of
    Left msg -> print msg
    Right d  -> putStrLn $ analyzeMidi d
    
    
