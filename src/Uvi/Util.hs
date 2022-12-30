{-# LANGUAGE TupleSections #-}

module Uvi.Util where

import qualified Data.Map as M
import Debug.Trace
import Text.Printf
import Data.Map(Map)
import Data.Maybe
import Control.Monad.State
import Uvi
import Common
import Score.ScoreData
import Instruments.InstrUtils
import Util.Exception
import Util.Map
import Common.CommonUtil




makePatchSections :: String -> Ut [PatchSection]
makePatchSections staffN = do
  score <- gets usScore
  let chords = stChords . uuMapLookup staffN . scStaves $ score
      firstLoc = case M.minViewWithKey chords of
        Nothing -> throwMine $ printf "makePatchSections: in staff %s, no chords" staffN
        Just ((loc1,_),_) -> loc1
      staff  = uuMapLookup staffN $ scStaves score
      marks  = scMarksByStaff score
      marksThisStaff = uuMapLookup staffN marks
      maybePatch :: Mark Double -> Maybe String
      maybePatch (Patch s) = Just s
      maybePatch _         = Nothing
      ms :: [(Loc,String)]
      ms     = M.toAscList $ markFilter "makePatchSections" maybePatch staffN marks
      locPairs :: [((Loc,String),(Loc,String))]
      locPairs = zip xs (tail xs) where xs = ms ++ [(stMaxTrueEnd staff,"")]
      g :: ((Loc,String),(Loc,String)) -> PatchSection
      g ((loc1,name),(loc2,_)) = PatchSection name staff filteredMap loc1 loc2 []
        where
          filteredMap = fst $ M.split loc2 $ snd $ split_LT_GE loc1 marksThisStaff
      sameTwoInRow = listToMaybe $ mapMaybe test locPairs
        where
          test t@((_,s1),(_,s2)) | s1 == s2  = Just t
                                 | otherwise = Nothing
      ret = case locPairs of
             [] -> throwMine $ printf "no patch marks on staff %s" staffN
             (x:xs) | firstLoc < fst (fst x) -> throwMine $ printf ("on staff %s, first " ++
                                                "chord is at loc %s, before first patch " ++
                                                "mark at %s") staffN (showLoc2 firstLoc)
                                                (showLoc2 . fst $ fst x)
                    | otherwise -> case sameTwoInRow of
                        
                        Just ((loc1,s1),(loc2,_)) -> throwMine $ printf ("at locs %s and " ++
                          "%s, there's the same patch mark (patch '%s')") (showLoc2 loc1)
                            (showLoc2 loc2) s1
                        Nothing -> map g (x:xs)
  return ret



-- returned list is in ascending order of Loc
makeChordData :: String -> Map Loc (Map Int Chord) -> [ChordData]
makeChordData staffN chords = map g $ concatMap f xs
  where
    xs :: [(Loc,Map Int Chord)]
    xs = M.toAscList chords
    f :: (Loc,Map Int Chord) -> [(Loc,(Int,Chord))]
    f (l,m) = map (l,) $ M.toAscList m
    g :: (Loc,(Int,Chord)) -> ChordData
    g (loc1,(vn,ch)) = ChordData staffN loc1 vn ch (cNotes ch)


uuMapLookup :: Ord k => k -> Map k a -> a
uuMapLookup k m = case M.lookup k m of
  Nothing -> throwMine "Uvi/Util.hs map lookup"
  Just x  -> x

