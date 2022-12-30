module Cac.Genetic.Util where

import qualified Data.List as L
import Control.Monad.State
import Text.Printf
import Cac.Genetic.Data
import Data.Function
import Data.Maybe
import Data.Monoid
import qualified Data.Set as S


showTime :: Rational -> String
showTime = error "foo"

showNote :: Note -> String
showNote n = printf "%s ->%s (%d)" (showTime $ nTBeg n) (showTime $ nTEnd n)
                                   (nId n)


showComp :: Comp -> String
showComp (Comp ns) = unlines $ map showNote ns


compFilterSounding :: Comp -> Comp
compFilterSounding (Comp ns) = Comp $ filter nSounding ns
  

compSum :: Comp -> Comp -> Comp
compSum (Comp ns1) (Comp ns2) =
  compMergeSimult $ compSort $ Comp (ns1 ++ ns2)


compSort (Comp ns) = Comp $ L.sortBy (compare `on` nTBeg) ns


compMergeSimult :: Comp -> Comp
compMergeSimult (Comp ns1) = compSort $ Comp $ filter filt $ map fst xs
  where
    filt :: Note -> Bool
    filt note = not $ (nId note) `S.member` iSet
    iSet = S.fromList $ concatMap snd xs
    xs :: [(Note,[Int])]
    xs = map g $ filter (not . null) $ L.tails ns1
    g :: [Note] -> (Note,[Int])
    g (noteIn:remain) = case vsC of
        ([],_) -> (noteIn,[])
        (ds,is) -> (noteIn {nTEnd = maximum ds},is)
      where
        vs :: [Maybe ([Rational],[Int])]
        vs = map (f (nTEnd noteIn) (nPit noteIn)) remain
        vsC :: ([Rational],[Int])
        vsC = mconcat $ catMaybes vs
    -- f <tEnd of source note> <pitch of source note> <question note>
    --   -> Maybe new end
    f :: Rational -> Int -> Note -> Maybe ([Rational],[Int])
    f t0 pit0 nQuery =
      if nTBeg nQuery <= t0 && nPit nQuery == pit0
        then Just ([nTEnd nQuery],[nId nQuery])
        else Nothing


nextId :: Ge Int
nextId = do
  s <- get
  let currI = gsNextId s
  put $ s { gsNextId = currI+1 }
  return currI



