import MidiFile.Convert

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

-- toNoteOn :: Int -> Int -> Ch.Body
-- toNoteOn p v = Ch.Voice (Vo.NoteOn (Vo.toPitch p) (Vo.toVelocity v))

-- e :: El.T ElapsedTime Ev.T
-- e = El.insert t1 body1 $ El.insert t2 body2 $ El.empty
--   where
--     -- empty = EventList.empty
--     t1 = 1
--     t2 = 1
--     chMsg1 = Ch.Cons (Ch.toChannel 1) (Ch.Voice (Vo.NoteOn (Vo.toPitch 60) (Vo.toVelocity 64)))
--     chMsg2 = Ch.Cons (Ch.toChannel 1) (error "foo")
--     body1 = Ev.MIDIEvent chMsg1 -- 
--     body2 = Ev.MIDIEvent chMsg2 -- Voice (Ev.NoteOff 60 64)

-- main2 = F.empty

-- main3 = do
--   F.Cons x y [trks] <- fromFile "demo.mid"
--   print $ length trks

main = putStrLn "test.mid"
