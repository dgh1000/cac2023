{-# LANGUAGE TemplateHaskell #-}

module Cac.Simpler.Lt where

import Control.Lens hiding (element)
import Control.Lens.Zoom

data Atom = Atom { _element :: String, _point :: Point } deriving (Show)

data Point = Point { _x :: Double, _y :: Double } deriving (Show)

data Molecule = Molecule { _atoms :: [Atom] } deriving(Show)




makeLenses ''Atom
makeLenses ''Point
makeLenses ''Molecule

shiftMoleculeX :: Molecule -> Molecule
shiftMoleculeX = over (atoms . traverse . point . x) (+1)


shiftAtomX :: Atom -> Atom
shiftAtomX = over (point . x) (+ 1)

-- LensLike' (Zoomed

main = do
  print $ shiftAtomX (Atom "hydro" (Point 1 1))
  
