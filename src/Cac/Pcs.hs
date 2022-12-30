{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}
module Cac.Pcs where


import qualified Data.Map as M
import qualified Data.List as L
import qualified Data.Set as S
import Data.Set(Set)
import Data.Maybe
import Data.Map(Map)
import Data.Word
import Data.Bits
import Util.Showable

-- PC set operation
data PcsOp = PcsOp Int (Maybe Int)   -- <maybe inversion, around given pitch>
                                   -- <transposition>

-- (0,1,3). the reason we store intervals is so we can look up whether 
data Pcs2 = Pcs2
  { psPitches   :: Set Int
  , psIntervals :: Map Int [(Int,Int)]
     -- map of an interval x (x <= 6) to each pair of pitches that are x apart
     -- (the short way around)
  }


type Pcs = Word16

----------------------------------------------------------------------

-- terminology
--
--   "instance of a pcset"
--
--     given a normalized pcs P, and a composition fragment C, an instance of
--     P is a subset of C that represents P in some T or TI.
--
--   "analysis map"
--
--     given a master pcs M, and a composition fragment C, an analysis map Map
--     Int [Pcs] is a cataloging of all instances of M or any subset of M into
--     in C, with pcs's then eliminated so that no pcs in the map is a direct
--     subset of any other in the map.
--
--



----------------------------------------------------------------------
--                         utilities


ordNub :: Ord a => [a] -> [a]
ordNub = S.toList . S.fromList


pFromList :: [Int] -> Pcs
pFromList = foldr g 0
  where
    g i w = setBit w (i `mod` 12)


pFromListT = pNormalizeT . pFromList


pFromListTI = pNormalizeTI . pFromList
  

pShow :: Pcs -> String
pShow = concatMap (\i -> show i++" ") . pToList


pToList :: Pcs -> [Int]
pToList w = concatMap g [0..11]
  where
    g i | testBit w i = [i]
        | otherwise   = []


pNormalizeT :: Pcs -> Pcs
pNormalizeT = minimum . take 12 . iterate pRotateDn


pNormalizeTI :: Pcs -> Pcs
pNormalizeTI w = min (pNormalizeT w) (pNormalizeT $ pInvert w 0)


pAllT :: Pcs -> [Pcs]
pAllT = ordNub . take 12 . iterate pRotateDn


pAllTI :: Pcs -> [Pcs]
pAllTI w = ordNub $ pAllT w ++ (pAllT $ pInvert w 0)


pInvert :: Pcs -> Int -> Pcs
pInvert wordIn mirrorBit
  | 0 <= mirrorBit && mirrorBit <= 11 = foldr (\i w -> g i .|. w) 0 [0..11]
  where
    g x | testBit wordIn x = bit (m x)
        | otherwise        = 0
    m x = (2*mirrorBit-x) `mod` 12


pRotateDn :: Pcs -> Pcs
pRotateDn w | x         = setBit   (shiftR w 1) 11
            | otherwise = clearBit (shiftR w 1) 11
  where x = testBit w 0


pCount :: Pcs -> Int
pCount s = length . filter (\p -> testBit p 0) . take 12 . iterate pRotateDn $
           s


-- Determine if w1 is a direct subset of w2.
pSubset :: Pcs -> Pcs -> Bool
pSubset w1 w2 = w1 .&. w2 == w1


-- Determine is s1 in any transposition is a direct subset of s2.
pSubsetT :: Pcs -> Pcs -> Bool
pSubsetT s1 s2 = any (flip pSubset $ s2) $ pAllT s1


-- Determine if s1 in any transposition or mirrored in any transposition is a
-- direct subset of s2.
pSubsetTI :: Pcs -> Pcs -> Bool
pSubsetTI s1 s2 = pSubsetT s1 s2 || pSubsetT (pInvert s1 0) s2


-- pStripOne
--
-- Given pcs 'x', find all ways of removing 1 pitch, normalize them, and
-- remove duplicates.
pStripOne :: Pcs -> Set Pcs
pStripOne x = S.fromList . map pNormalizeTI . mapMaybe g $ pAllTI x
  where
    g :: Pcs -> Maybe Pcs
    g w | testBit w 0 = Just $ clearBit w 0
        | otherwise   = Nothing


-- pFindInstances
--
--   Given set 'motif', find and return instances over TI trans/inv in 'frag'
--

pFindInstances :: Pcs -> Pcs -> [Pcs]
pFindInstances motif frag = filter (flip pSubset frag) $ pAllTI motif
          
----------------------------------------------------------------------


-- analyzeFrag
--
--   Input: to use this function, call analyzeSubsets on your motif first, and
--   pass that as first argument.
--
--   Given a PCS called the "motif," and given a PCS called a "frag" (because
--   it's a fragment of a larger composition), produce an analysis of
--   instances or partial instances of 'motif' occurring within 'frag'.
--
--   Output: Map Int [Pcs], which is a map of Pcs size to list of concrete
--   instances of a Pcs of that size. No Pcs in this output map is a subset of
--   any other.
--
--   algorithm:
--
--    Call analyzeSubsets on 'motif' to produce sub_map :: Map Int (Set Pcs).
--
--    Produce ss :: [[Pcs]] by arranging the 'Set Pcs' in sub_map in
--    descending order of size, and converting each 'Set Pcs' to [Pcs]
--
--    Initialze Z :: Set Pcs to empty.
--
--    for each entry e=[Pcs] in 'ss':
--
--      for each s <- e:
--
--        s is a normalized Pcs. We need to find all instances in 'frag' over
--        T and I. (Later we might not look for instances over I.) Construct
--        'insts', the set of all instances in 'frag'.
--
--        filter out of 'insts' any Pcs that is a subset of something in Z
--        already. call results insts2.
--
--        add everything in insts2 to Z.
--
--    Split Z into different sizes and construct result map.
--
analyzeFrag :: Map Int (Set Pcs) -> Pcs -> Map Int [Pcs]
analyzeFrag motifSubsets frag = M.fromListWith (++) . map sp $ foldl g [] ss
  where
    sp w = (pCount w,[w])
    ss :: [Pcs]
    ss = concatMap (S.toList . snd) $ M.toDescList motifSubsets
    g :: [Pcs] -> Pcs -> [Pcs]
    g listIn s = listIn++xs
      where
        xs = filter (\c -> not $ isInListAlready c) $ pFindInstances s frag
        isInListAlready x = any (pSubset x) listIn

    -- given an analysis map which includes all meaningful instances of size
    -- s > n, do this
    --
    --   find all subsets of motif of size n in any transposition or
    --   inversion. nub them.
    --
    --   for each one, check if it's in the comp
    --
    --   if it is, then check if it's a subset of any pcs of any size
    --   currently in the input analysis map. if it's not, then add it to the
    --   analysis map.
    -- 
    



-- given 'motif', produce
--
--   Map Int (Set Pcs)
--
-- which is a map of 'n' (a *pc set size*) to a Set of all pcsets of that size
-- which are subsets of 'motif'. All the pcsets in each Set are normalized.
--
-- The largest Set contains just 'motif' itself (normalizaed)
-- 
analyzeSubsets :: Pcs -> Map Int (Set Pcs)
analyzeSubsets motif
    = M.fromList $ L.unfoldr f (S.singleton $ pNormalizeTI motif)
  where
    c = pCount motif
    f :: Set Pcs -> Maybe ((Int,Set Pcs),Set Pcs)
    f s | n < 2     = Nothing
        | otherwise = Just ((n,s), pStripOne_set s)
      where
        pcs1 = case S.maxView s of {Just (x,_) -> x}
        n = pCount pcs1

    
pStripOne_set :: Set Pcs -> Set Pcs
pStripOne_set s = S.unions . map pStripOne $ S.toList s


----------------------------------------------------------------------
--                       showing analysis results of analyzeFrag


type PcsAnalysis = Map Int [Pcs]

instance ShowItemClass PcsAnalysis where
  showI m = Component "PcsAnalysis" True (map g $ M.toDescList m)
    where
      g :: (Int,[Pcs]) -> ShowItem
      g (n,ps) = Component (show n) True (map h ps)
      h :: Pcs -> ShowItem
      h = SingleLine . concatMap (\i -> show i++" ") . pToList
  

----------------------------------------------------------------------
--                    pitch class to pitch via register


-- PtpvrNonEqualSpacing <closer pitch>
--   <further pitch> <lower pitch> <higher pitch>
--
-- PtpvrEqualSpacing <lower pitch> <higher pitch>
data PcToPitchViaRegister = PtpvrExact Int
                          | PtpvrNonEqualSpacing Int Int Int Int
                          | PtpvrEqualSpacing Int Int


-- pcToPitchViaRegister
--
--   Given a pitch class PC and a general register R, find P such that
--
--     If R is not of pitch class PC, find the two pitches P1 and P2 that are
--     the closest lower and higher pitches of class PC.
--
--     Floor ceiling.
--
--
pcToPitchViaRegister :: Int -> Int -> PcToPitchViaRegister
pcToPitchViaRegister pc register = out
  where
    -- we have 64 and pc is 2 . find pc of 64-2 = 62. is round 62 to 60.
    --
    -- we have 2. round to 6.2 or 7.2 or 8.2, subtract 0.2 round
    d :: Double
    d = fromIntegral register/12 - fromIntegral pc/12
    drCeil  = 12 * ceiling d + pc
    drFloor = 12 * floor d + pc
    
    out = case (register-drFloor,drCeil-register) of
      (0,0) -> PtpvrExact drCeil
      (x,y) | x == y -> PtpvrEqualSpacing drFloor drCeil
            | x <  y -> PtpvrNonEqualSpacing drFloor drCeil drFloor drCeil
            | x >  y -> PtpvrNonEqualSpacing drCeil drFloor drFloor drCeil 

