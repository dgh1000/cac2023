
module Instruments.Moog2 where

import qualified Data.Map.Strict as M
import Instruments.InstrumentsData

-- moog organization
--
--   Reaper/Logic TRACKS: each one holds a MOOG INSTANCE configured to a
--   particular PATCH (wiring arrangement).
--
--   Each TRACK receives **all midi channel** data for one MIDI input.
--
--   A Moog instance may sound different depending on the settings of its
--   controls; a set of control values together with the specific patch is an
--   ARTICULATION.
--
--   each TRACK will be fed from one MIDI STREAM... the MOOG INSTANCE at that
--   track will be configured to receive control and note messages from all 16
--   tracks.
--
-- MOOG META-INSTRUMENT
--
--   will play notes from a SET of one or more STAVES IN SIBELIUS.
--
--   will direct those notes to a **set** of one or more MOOG INSTANCES/
--
-- data we need to keep with each Moog
--
--   control definitions:
--
--     example: "lo-pass frequency" -- we will store a channel and MIDI
--     controller number
--
--     there is a master list of controls. these will apply to any Moog PATCH
--
--   articulation definition
--
--     contains patch name and control values
--
--   MoogTrack
--
--     contains stream number and patch name
--
-- control points: curves are associated with a particular staff, but a staff
-- could potentially be directed to different tracks or channels. so we need
-- to decide what stream to send a curve to.. for now, to all.
--
-- CONTROL CURVES
--
--   how configured...
--
--     a Moog is created with a function 

mg = Moog [] M.empty [] [] 

mg2 = MetaInstr "moog2" [] mg moogRun 


-- moogRun
--
--   
moogRun :: MetaInstr -> Moog -> Int -> Int -> Tr MetaInstr
moogRun mi (Moog iraws patches ctrlMap articMap) begMsr endMsr = do
  
  allStaffLowLevel
