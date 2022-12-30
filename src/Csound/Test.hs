module Csound.Test where

import Translation

foo :: Int
foo = 3

data CsoundInstr = CsoundInstr
  { cint :: Int }

makeCsound :: String -> [String] -> MetaInstr
makeCsound name staffNs = MetaInstr name staffNs (error "foo") True (error "foo") 
                          (error "foo")

runCsound :: MetaInstr -> CsoundInstr -> MetaPrepared -> Tr ()
runCsound meta cs metaPre = do
  error "foo" 