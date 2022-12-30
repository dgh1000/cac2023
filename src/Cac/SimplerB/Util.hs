{-# LANGUAGE TupleSections, MultiParamTypeClasses, FunctionalDependencies #-}

-- we've got leit motifs.
--
-- repeat 3. slight down. big down. (2-4 of one) (1-3 of another)
--
-- repeat 3 to 5
--
-- how do you fit a pattern to a leit motif? how do you guarantee variety?
--
-- then leit motives can be repeated in a certain pattern. composite motif. n
-- long. n_2 long. n long n long dramatic form, bigger form. louder note fit
-- with drama.
--
-- 
--
-- 

module Cac.SimplerB.Util where

import Data.Array.IArray

computeEvenness :: [Int] -> Int -> Int -> Double
computeEvenness values minV maxV = error "computeEvenness"
  where
    arr :: Array Int Double
    arr = array (minV,maxV) $ map (,0) [minV..maxV]

class MotifData a b | a -> b where
  length   :: a -> Int
  strength :: a -> Double
  isMatch  :: a -> b -> Bool
  

-- match is a kind of parsing. m
match :: Motif a -> [b] -> Maybe (Motif a)
match = error "foo"
  where


type RepElem a = ((Int,Int),a)

data MatchState = MatchState Int -- elem # and reps so far

match' :: RepElem a -> MatchState -> [b] -> (Maybe MatchState,[b])
match' ((minM,maxM),a) st b = error "match'"
  where


    
descend :: Motif a -> [RepElem a]
descend (MOne a) = [((1,1),a)]
descend (MComposite ms) = concatMap descend ms
descend (MRepeat (minR,maxR) a) = [((minR,maxR),a)]

                                  
data Motif a = MComposite [Motif a]
             | MOne a
             | MRepeat (Int,Int) a
