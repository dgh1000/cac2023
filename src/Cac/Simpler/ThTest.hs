{-# LANGUAGE MultiParamTypeClasses, FunctionalDependencies #-} 

module Cac.Simpler.ThTest where

class Selectable s b | s -> b where
  selectName :: s -> String -> b

(|+|) :: (Selectable s b,Selectable s2 b2) => s -> String -> s2
(|+|) x name = selectName x name
