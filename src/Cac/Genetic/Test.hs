module Cac.Genetic.Test where

import Cac.Genetic.Data
import Cac.Genetic.Util
import Cac.Genetic.Serial
import Cac.Genetic.Show
import Util.Showable

scale01 = [0, 2, 3, 5, 6, 8, 9, 11]

mes :: [MotiveElem PitScaleDegree]
mes = [ MeNote (PitScaleDegree 2 1 scale01) 1 1 
      , MeNote (PitScaleDegree 2 1 scale01) 1 1 ]


mkNote id_ t pit = Note id_ t (t+1) Nothing pit 3.0 True

n1 = mkNote 0 0.0 60
n2 = mkNote 1 0.5 61
n3 = mkNote 2 1.0 60
n4 = mkNote 3 0.5 65
n5 = mkNote 4 0.5 66

c1 = compSort $ Comp [n1,n2,n3,n4,n5]

main3 = do
  putStrLn $ showComp c1
  putStrLn $ showComp $ compMergeSimult c1


main2 = do
  writeComp "test01.comp" c1
  readComp "test01.comp" >>= putStrLn . showComp


main = do
  putStrLn $ showIString $ MotiveElemList mes


