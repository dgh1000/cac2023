
module Score.FindEnd where

import qualified Data.Set as S
import qualified Data.List as L
import qualified Data.Map as M
import Debug.Trace
import Data.Map(Map)
import Data.Maybe
import Text.Printf
import Data.Set(Set)
import Data.Function
import Score.ScoreData
import Util.Exception
import Common 

----------------------------------------------------------------------
--                 finding measure range

{-
findEndMsr :: Int -> Score -> Int
findEndMsr begMsr score
  | begMsr > (snd $ A.bounds a) =
      throwMine ("Given beginning measure is past "++
                 "the end of the score")
  | otherwise = 
      let x = searchNBlanks begMsr 3 . drop (begMsr-1) . 
              (++ [False,False,False,False]) . A.elems $ a
      in ("End measure:" ++ show x) `trace` x
  where
    a = scUsedMsrs score


searchNBlanks :: Int -> Int -> [Bool] -> Int
searchNBlanks currMsr n bs
  | L.isPrefixOf (replicate n False) bs = currMsr - 1
  | otherwise = searchNBlanks (currMsr+1) n (drop 1 bs)
-}


data Clump = Clump { clBeg :: Int, clEnd :: Int, clSize :: Int, clFlag :: Bool }

-- findEndMsr
--
findEndMsr :: Int -> [String] -> Score -> Int
findEndMsr begMsr1 splicePts score =   
  case map (findEndMsr2 $ scUsedMsrs score) (begMsr1:spliceMsrs) of 
    [] -> error "famn39b8"
    xs -> maximum xs 
  where
    spliceMsrs :: [Int]
    spliceMsrs = findSpliceMsrs splicePts score


findSpliceMsrs :: [String] -> Score -> [Int]
findSpliceMsrs splicePts score =
  let xs = map perPoint splicePts
  in printf "splice measures: %s" (show xs) `trace` xs
  where
    perPoint :: String -> Int
    perPoint pointName = msrNum . snd $ lookupSplicePoint pointName score


lookupSplicePoint :: String -> Score -> (Loc,Loc)
lookupSplicePoint pointName score = case (sb2,eb2) of
    ([b],[e]) -> (b,e)
    _         -> throwMine $ printf ("in looking up splice points " ++
                 "with name '%s', didn't find a single begin and end " ++
                 "splice point. (either found more than one, or none, " ++
                 "of begin/end") pointName
  where    
    m1 :: [Map Loc [MarkD]]
    m1 = M.elems $ scMarksByStaff score
    mSpliceBegAtLoc :: (Loc,[MarkD]) -> Maybe Loc
    mSpliceBegAtLoc (loc,ms) = case filter (bSpliceBeg pointName) ms of
      []  -> Nothing
      [_] -> Just loc
    mSpliceEndAtLoc :: (Loc,[MarkD]) -> Maybe Loc
    mSpliceEndAtLoc (loc,ms) = case filter (bSpliceEnd pointName) ms of
      []  -> Nothing
      [_] -> Just loc
    sb2 :: [Loc]
    sb2 = catMaybes $ concatMap (map mSpliceBegAtLoc . M.toAscList) m1 
    eb2 = catMaybes $ concatMap (map mSpliceEndAtLoc . M.toAscList) m1


bSpliceBeg :: String -> MarkD -> Bool
bSpliceBeg s (SpliceBeg b) = s == b
bSpliceBeg _ _             = False
    

bSpliceEnd :: String -> MarkD -> Bool
bSpliceEnd s (SpliceEnd b) = s == b
bSpliceEnd _ _             = False
    


-- findEndMsr2
--
findEndMsr2 :: Set Int -> Int -> Int 
findEndMsr2 msrSet begMsr
  | begMsr > maxMsr = throwMine "given beginning msr is past end of score"
  | otherwise       = e
  where
    maxMsr :: Int
    maxMsr = case S.maxView msrSet of {Just (x,_) -> x}
    f :: Int -> (Int,Bool)
    f i = (i,S.member i msrSet)
    l2 :: [Clump]
    l2 = case map toClump $ L.groupBy ((==) `on` snd) $
           map f [begMsr..maxMsr] of
             ls@(_:_) -> ls
    toClump :: [(Int,Bool)] -> Clump
    toClump xs@((_,flag):_) = Clump m1 m2 (m2-m1+1) flag
       where
         m1 = minimum $ map fst xs
         m2 = maximum $ map fst xs
    Clump _ e _ _ = case takeWhile (\c -> clFlag c || clSize c < 3) l2 of
      [] -> throwMine $ printf ("in seraching for end measure, " ++
            "starting at %d (which "++
            "may be either explicit start measure or splice point), " ++
            "found only empty measures") begMsr
      xs -> last xs
    


test :: IO ()
test = do
  let s1 = S.fromList [1,2,3,6,7,11]
  putStrLn $ show $ findEndMsr2 s1 (1)
