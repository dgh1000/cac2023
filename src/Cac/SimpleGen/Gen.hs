{-# LANGUAGE ScopedTypeVariables #-}
module Cac.SimpleGen.Gen where

import Cac.SimpleGen.Data
import Util.RandMonad

makeClump :: forall a. [(a,Int)] -> RMon (Clump a)
makeClump inp = do
  let g :: (a,Int) -> [a]
      g (x,n) = replicate n x
  Clump <$> (rPermuteList $ concatMap g inp)


makeNote :: Int -> PitC -> RegisterC -> DurC -> SpanC -> RMon Note
makeNote tIn pitIn regIn durIn spanIn = do
  -- where did we leave off? 
  error "foo"

  
