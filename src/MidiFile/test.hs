
import Sound.MIDI.IO
import Sound.MIDI.File.Load
import Sound.MIDI.File
import qualified Data.EventList.Relative.TimeBody as EL
import Sound.MIDI.File.Event as Ev

{-
doSomethingWithTrack :: Track -> IO ()
doSomethingWithTrack (EL.
-}

main = do
  Cons x y trks <- fromFile "demo.mid"
  print $ length trks
