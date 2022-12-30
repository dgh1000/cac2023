import Codec.Midi

track0 = [ (0,  NoteOn 0 60 80)
         , (24, NoteOff 0 60 0)
         , (0,  TrackEnd) ]

track1 = [ (0,  NoteOn 0 64 80)
         , (24, NoteOn 0 64 0)
         , (0,  TrackEnd) ]
        
myMidi = Midi { fileType = MultiTrack, 
                timeDiv  = TicksPerBeat 24, 
                tracks   = [track0, track1] }         

main = do
  exportFile "codec.mid" myMidi
  
