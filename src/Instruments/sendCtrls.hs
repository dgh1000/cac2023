-- needs to be a program it compiles. 

controlSet01 :: ControlSet
controlSet01 = [ ((1, 103), 10)  -- filter cutoff
               
               , ((1, 104), 64)  -- filter cutoff mod intensity: linear key
                                 --   follow

               -- filter cutoff ADSR
               , ((1, 105), 64)  -- attack
               , ((1, 106), 64)  -- decay
               , ((1, 107), 64)  -- sustain
               , ((1, 108), 64)  -- release
               , ((1, 114), 64)  -- dynamic
               , ((1, 115), 64)  -- intensity, modulation of cutoff

               -- amp ADSR

               , ((1, 109), 64)  -- attack
               , ((1, 110), 64)  -- decay
               , ((1, 111), 64)  -- sustain
               , ((1, 112), 64)  -- release
               , ((1, 113), 64)  -- dynamic

               
               ]

controlSets :: [ControlSet]
controlSets = [controlSet01]
