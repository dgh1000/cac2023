

import Cac.MusicXml
import Cac.MusicXml.Elems

attr1 = MxMsrAttr 256 0 (MxTimeSig 4 4) MxcTreble

item1 = MmiNote (MxPitch 0 5) 256 1 Mnt4

m1 = MxMsr 1 attr1 

main = putStrLn "foo"
