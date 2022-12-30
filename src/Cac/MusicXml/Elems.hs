module Cac.MusicXml.Elems where

import Text.XML.Light.Types
import Text.XML.Light.Output
import Cac.MusicXml

--              SKIPPING!!
-- <identification>
-- <defaults>
-- within <part-list>
--
--   skipping <part-group>



work :: Element
work = simpleElem "work" [] [Elem $ simpleElem "work-title" [] []]

scorePartwise :: MyScorePart -> Element
scorePartwise msp = simpleElem "score-partwise"
                [Attr (simpleQ "version") "3.0"]
                [ Elem work
                , Elem $ partList msp ]


partList :: MyScorePart -> Element
partList sp = simpleElem "part-list" []
  [ Elem $ scorePart sp
  , Elem $ scoreInstr "I1" (mspInstrName sp) ] 


{-
partGroup :: Element
partGroup = error "Foo"
-}

scorePart :: MyScorePart -> Element
scorePart sp = simpleElem "score-part" [] [Elem partName]
  where
    partName = simpleElem "part-name" [] [simpleContent "piano"]


scoreInstr :: String -> String -> Element
scoreInstr instrId name =
    simpleElem "score-instrument" [simpleAttr "id" instrId]
               [Elem instrName, Elem instrSound]
  where
    instrName = simpleElem "instr-name" [] [simpleContent name]
    instrSound = simpleElem "instrument-sound" []
                 [simpleContent "keyboard.piano.grand"]

{-    
main = do
  let elem = scorePartwise msp
      msp = MyScorePart "P1" "piano" "piano"
  writeFile "m.xml" $ ppElement elem
-}
