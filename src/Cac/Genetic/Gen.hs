
module Cac.Genetic.Gen where

import qualified Data.List as L
import Cac.Genetic.Data
import Cac.Genetic.Util
import Util.RandMonad
import Data.Maybe

----------------------------------------------------------------------

genFromMotive :: MotiveClass a => Rational -> [MotiveElem a] -> Ge Comp 
genFromMotive tBeg elems = do
  ns <- nextNotes tBeg elems
  return $ Comp ns


mkNote :: MotiveClass a => Rational -> Rational -> a -> Ge Note
mkNote t dur abstractPit = do
  i <- nextId
  return $ Note i t (t+dur) Nothing (toPitch abstractPit) 4.0 True

        
nextNotes :: MotiveClass a => Rational -> [MotiveElem a] -> Ge [Note]
nextNotes t0 (MeNote pit dur span:remain) = do
  n <- mkNote t0 dur pit
  ns <- nextNotes (t0+span) remain
  return $ n:ns
nextNotes t0 (MeRest span:remain) = nextNotes (t0+span) remain
nextNotes _ [] = return []  


----------------------------------------------------------------------


data OneSlot = OsEmpty
             | OsSound Bool Int


data Slots = Slots [OneSlot]


genRandomSlots :: Int -> (Int,Int) -> Ge Slots
genRandomSlots nEmpties (r1,r2) = do
  let np = r2-r1+1
      ns :: [Bool]
      ns =  replicate np True ++ replicate nEmpties False
  nsp <- rPermuteList ns
  let mis :: [Maybe Int]
      mis = grs_help [r1..r2] nsp
  error "foo"


grs_help :: [Int] -> [Bool] -> [Maybe Int]
grs_help [] _             = []
grs_help (p:ps) (True:bs) = Just p  : grs_help ps bs
grs_help ps (False:bs)    = Nothing : grs_help ps bs


-- shiftRandomSlots
--
--   Somehow shift the postions of empties. Pick a starting point and shift as
--   much as possible. Go to one direction and determine where first hole is.

shiftRandomSlots :: Double -> Ge Slots -> Ge Slots
shiftRandomSlots amount = error "foo"


findHoleRight :: Int -> [Maybe Int] -> Int
findHoleRight idx1 elems = result
  where
    cs :: [(Int,Maybe Int)]
    cs = zip [0..] elems
    (_,rs) = L.splitAt idx1 cs
    xxs = L.dropWhile (\(_,c) -> isJust c) rs
    result = case xxs of
      []       -> -1
      (i2,_):_ -> i2



