
import Sound.MIDI.IO
import Sound.MIDI.File.Load
import Sound.MIDI.File
import qualified Data.EventList.Relative.TimeBody as EL
import Sound.MIDI.File.Event as Ev

{-
doSomethingWithTrack :: Track -> IO ()
doSomethingWithTrack (EL.
-}

-- type Track = EventList.T ElapsedTime Event.T

-- T: Event
-- data T =
--      MIDIEvent       ChannelMsg.T
--    | MetaEvent       MetaEvent.T
--    | SystemExclusive SysEx.T
--      deriving (Show,Eq,Ord)

-- empty :: T (MidiFile)
-- empty = Cons Mixed (Ticks 1) [EventList.empty]

-- 
-- type TrackEvent = (ElapsedTime, T) T=Event

tryWrite = do
  return ()

main = do
  Cons x y trks <- fromFile "demo.mid"
  print $ length trks
