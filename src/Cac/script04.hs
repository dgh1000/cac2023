main = do
  print $ pcToPitchViaRegister 2 69


data PcToPitchViaRegister = PtpvrExact Int
                          | PtpvrNonEqualSpacing Int Int Int Int
                          | PtpvrEqualSpacing Int Int
                          deriving(Show)

pcToPitchViaRegister :: Int -> Int -> PcToPitchViaRegister
pcToPitchViaRegister pc register = out
  where
    -- we have 64 and pc is 2 . find pc of 64-2 = 62. is round 62 to 60.
    --
    -- we have 2. round to 6.2 or 7.2 or 8.2, subtract 0.2 round
    d :: Double
    d = fromIntegral register/12 - fromIntegral pc/12
    drCeil  = 12 * ceiling d + pc
    drFloor = 12 * floor d + pc
    
    out = case (register-drFloor,drCeil-register) of
      (0,0) -> PtpvrExact drCeil
      (x,y) | x == y -> PtpvrEqualSpacing drFloor drCeil
            | x <  y -> PtpvrNonEqualSpacing drFloor drCeil drFloor drCeil
            | x >  y -> PtpvrNonEqualSpacing drCeil drFloor drFloor drCeil 

  
