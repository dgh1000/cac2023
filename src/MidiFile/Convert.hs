module MidiFile.Convert where

import Sound.MIDI.IO
import Sound.MIDI.File.Load
import Sound.MIDI.File.Save as Sa
import Sound.MIDI.File as F
import qualified Data.EventList.Relative.TimeBody as El
import Sound.MIDI.File.Event as Ev
import Sound.MIDI.Message.Channel as Ch
import Sound.MIDI.Message.Channel.Voice as Vo
import Sound.MIDI.File.Event.Meta as Me
import qualified Sound.MIDI.File.Save as Sa
import Midi.MidiData

-- okay now we have to select just one staff. Can do that in the call to RunOnce? 
-- 

{-
doSomethingWithTrack :: Track -> IO ()
doSomethingWithTrack (EL.
-}


-- ElapsedTime is an Integer
-- 
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

-- ChannelMsg
-- data T = Cons {
--      messageChannel :: Channel,
--      messageBody    :: Body
--    }

-- newtype Channel = Channel {fromChannel :: Int} deriving (Show, Eq, Ord, Ix)

-- data Body =
--      Voice Voice.T
--    | Mode  Mode.T
--      deriving (Show, Eq, Ord)

-- Voice
-- data T =
--      NoteOff        Pitch Velocity
--    | NoteOn         Pitch Velocity
--    | PolyAftertouch Pitch Pressure
--    | ProgramChange  Program
--    {-
--    Shall we add support for registered parameters?
--    -}
--    | Control        Ctrl.T ControllerValue
--    | PitchBend      PitchBendRange
--    | MonoAftertouch Pressure

-- Event List
-- insert :: (Ord time, Ord body) => time -> body -> T time body -> T time body 


type StdEl = El.T ElapsedTime Ev.T


toNoteOn :: Int -> Int -> Ch.Body
toNoteOn p v = Ch.Voice (Vo.NoteOn (Vo.toPitch p) (Vo.toVelocity v))


toNoteOff :: Int -> Int -> Ch.Body
toNoteOff p v = Ch.Voice (Vo.NoteOff (Vo.toPitch p) (Vo.toVelocity v))


-- insertNote :: Integer -> Integer -> Int -> Int -> Int -> StdEl -> StdEl
insertNote tOn tOff chan note vel = El.insert tOn body1 .  El.insert tOff body2
  where
    chMsg1 = Ch.Cons (Ch.toChannel chan) (toNoteOn note vel)
    chMsg2 = Ch.Cons (Ch.toChannel chan) (toNoteOff note vel)
    body1 = Ev.MIDIEvent chMsg1
    body2 = Ev.MIDIEvent chMsg2


insertNoteOn t chan note vel = El.insert t body
  where
    chMsg = Ch.Cons (Ch.toChannel chan) (toNoteOn note vel)
    body = Ev.MIDIEvent chMsg


insertNoteOff t chan note vel = El.insert t body
  where
    chMsg = Ch.Cons (Ch.toChannel chan) (toNoteOff note vel)
    body = Ev.MIDIEvent chMsg


insertCtrlMessage t chan controller val = El.insert t body
  where
    chMsg = Ch.Cons (Ch.toChannel chan) v
    body = Ev.MIDIEvent chMsg
    ctrl = Ch.toController controller
    v = Ch.Voice (Vo.Control ctrl val) 


-- notesVels = [(1000,2100,1,60,96),(2000,3100,1,62,84),(3000,4100,1,64,72),(4000,5100,1,65,60)]

-- f = F.Cons F.Mixed (Ticks 1000) [track]
--   where
--     -- g :: StdEl -> (Integer,Int,Int,Int) -> StdEl
--     g el (time1,time2,chan,pitch,vel) = insertNote time1 time2 chan pitch vel el
--     track = foldl g El.empty notesVels

insertFileMessage (FMNoteOn t chan note vel) = insertNoteOn (round $ 1000.0 * t) chan note vel
insertFileMessage (FMNoteOff t chan note vel) = insertNoteOff (round $ 1000.0 * t) chan note vel
insertFileMessage (FMCtrl t chan ctrl value) = insertCtrlMessage (round $ 1000.0 * t) chan ctrl value

writeFileMessages :: FilePath -> [FileMessage] -> IO ()
writeFileMessages fp messages = Sa.toFile fp f
  where
    f = F.Cons F.Mixed (Ticks 1000) [track]
    track = foldr insertFileMessage El.empty messages

-- Sound.MIDI.File
-- F.Cons Type Division [Track]	 
-- from Sound.MIDI.File
-- empty = Cons Mixed (Ticks 1) [EventList.empty]

test2 = do
  F.Cons x y [trks] <- fromFile "demo.mid"
  print $ length trks

