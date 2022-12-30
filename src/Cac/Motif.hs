
module Cac.Motif where



data Motif = Motif [MotifUnit]

data MotifUnit = MotifUnit MotifPit MotifDur

data MotifPit = MotifPit MotifChordId MotifPitLev

data MotifPitLev = MplRepeat
                 | MplLeap Int Int
                 | MplTess Double   -- ranges from -1 to 1
                 | MplAbs  Int


data MotifChordId = MotifChordId


data MotifDur = MdBeats Rational
              | MdTess Double


-- what are some examples of motifs? outline of notes with certain kinds of filler between
-- them. we could put more constraints on filler, but that could also come from

-- a motif is a special case of "recognizable, recurring patterns". A pattern could be
-- something within a single melody: or it could be a particular note is followed by another
-- note in any voice. a pattern can be strict: always up then down. or it could be "generally
-- up for a few notes, then down for a few (less)"
--
-- patterns can be generative or recognized through search. we need to make decisions in clumps
-- in order to. we need to lay down notes or rhythms in a block. how could we lay down a bunch
-- of pitches or rhythms at once?
--
--   if we have a melodic motif, we 



        
