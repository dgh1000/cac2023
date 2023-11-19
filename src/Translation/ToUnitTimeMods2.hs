{-# LANGUAGE TupleSections #-}

module Translation.ToUnitTimeMods2 where


-- boundaries/adjusts: how do I know which direction to ramp? 1 on the side of
-- smaller absolute adjust?



import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
import Data.Function
import Debug.Trace
import Control.Monad.State
import Text.Printf
import Control.Monad
import Data.Either
import Data.Map.Strict(Map)
import Data.Set(Set)
import Data.Maybe
import Data.Monoid
import Score.ScoreData
import Translation
import Common
import Common.CommonUtil
import Util.Exception
import Util.Map
import Data.Ratio

-- can these be expressed in unit time mods? something like accel a certain
-- amount but adjust for absolute time that must be matched.

-- also pause is elemental.

-- we need to decide order of time mods for instance. 

-- we sometimes need to look up w's or other markers. on first pass pair
-- certain Marks with w's or whatever

-- it would be helpful to sort Marks first or extract

-- pair every mark with


-- things that are changed to unit time mods

-- staffAdjusts, globalAdjusts, warps, abs warps, ramps


-- how do I tweak ramps? similar

-- need to find 

----------------------------------------------------------------------
----------------------------------------------------------------------


computeDirectUtms :: Map Loc (Map String [MarkD]) -> [Utm]
computeDirectUtms = computeUtms2 . computeTmMarks

computeTmMarks :: Map Loc (Map String [MarkD]) ->
                  Map Loc (Map String [TimeModMark])
computeTmMarks = M.mapMaybeWithKey g
  where
    g loc m1 | M.null x  = Nothing
             | otherwise = Just x
      where x = lMapMaybe (f loc) m1
    f :: Loc -> MarkD -> Maybe TimeModMark
    f l (AbsWarp ws amt) = Just $ TmmAbsWarp ws amt
    f l (Pause amt)      = Just $ TmmPause amt
    f l W                = Just TmmW  
    f l (Boundary2 mAmt)     = Just $ TmmBoundary mAmt
    -- f l (AdjustL dur amt) = Just $ TmmAdjust LeftWarp dur amt
    -- f l (AdjustR dur amt) = Just $ TmmAdjust RightWarp dur amt
    f l (PostPause amt)   = Just $ TmmAfterPause amt
    f l (LeftSideShift dur amt) = Just $ TmmShift LeftWarp dur amt
    f l (RightSideShift dur amt) = Just $ TmmShift RightWarp dur amt
    f _ _ = Nothing


-- computeUtms2
--
computeUtms2 :: Map Loc (Map String [TimeModMark]) -> [Utm]
computeUtms2 m1 = concatMap g $ M.toList m1
  where
    g :: (Loc,Map String [TimeModMark]) -> [Utm]
    g (loc,m2) = concatMap k staffMarks
      where
        staffMarks :: [(String,[TimeModMark])]
        staffMarks = M.toAscList m2
        k :: (String,[TimeModMark]) -> [Utm]
        k (staffN,marks) = concatMap (toUtm1 loc staffN) marks
        

type TmmMap = Map String (Map Loc [TimeModMark])


-- toUtm1
--
--   Convert a single TimeModMark to one or more Utms.
--
--   Inputs: Loc of mark, 

-- toUtm1 :: Loc -> String -> TimeModMark -> [Utm]
-- toUtm1 loc staffN t@TmmShift {} = printf "%s: %s %s" (showLoc2 loc) staffN (show t) `trace`
--   toUtm1' loc staffN t
-- toUtm1 a b c = toUtm1' a b c

toUtm1 :: Loc -> String -> TimeModMark -> [Utm]
toUtm1 loc staffN (TmmAbsWarp _ _) = 
  throwMine $ printf "%s: absolute warp not implemented" (showLoc2 loc)
toUtm1 loc staffN (TmmPause amt) = [UtmPause Nothing loc amt]
toUtm1 loc staffN (TmmAfterPause amt) = [UtmPostPause Nothing loc amt]
toUtm1 loc staffN (TmmShift side dur amt) = case side of
  LeftWarp -> [UtmLShift (Just staffN) loc dur amt]
  RightWarp -> [UtmRShift (Just staffN) loc dur amt]
toUtm1 _ staffN TmmW = []
toUtm1 loc staffN (TmmBoundary _) =
  throwMine $ printf "%s: boundaries not implemented" (showLoc2 loc)
toUtm1 loc staffN (TmmAdjust LeftWarp dur amt) =
  [UtmLShift (Just staffN) loc dur amt]
toUtm1 loc staffN (TmmAdjust RightWarp dur amt) =
  [UtmRShift (Just staffN) loc dur amt]


-- utmRank
--
--   Assign an integer to each type of Unit time mod, such that lower
--   interger means it should be applied first, and higher integer means
--   last.
--
--   Ramp 
utmRank :: Utm -> Int
utmRank UtmRamp {}                  = -1
utmRank (UtmWarp _ _ _ _ (Left _))  = 0
utmRank UtmPause {}                 = 1
utmRank UtmPostPause {}             = 2
utmRank (UtmWarp _ _ _ _ (Right _)) = 3
utmRank UtmLShift {}                = 4
utmRank UtmRShift {}                = 5

  
---------------------------------------------------------------------
---------------------------------------------------------------------
--  Time adjusts on relative time map. As of Aug 29, 2023, intended
--  for use on staff time maps rather than global base map

-- computeAdjustUtms m = unlines (map show out) `trace` out
--   where
--     out = computeAdjustUtms' m 



computeAdjustUtms :: Map Loc (Map String [MarkD]) -> [Utm]
computeAdjustUtms = concat . M.elems . g5 . g4 . g3b . g3 . g2 . g1
  where
    g1 :: Map Loc (Map String [MarkD]) -> Map Loc [(String,[MarkD])]
    g1 = M.map M.toAscList
    g2 :: Map Loc [(String,[MarkD])] -> [(Loc,(String,[MarkD]))]
    g2 = expandTuples . M.toAscList
    g3 :: [(Loc,(String,[MarkD]))] -> Map String [(Loc,[MarkD])]
    g3 = M.fromListWith (++) . map (\(a,b) -> (a,[b])) . flipLocStaffN
    g3b :: Map String [(Loc,[MarkD])] -> Map String [(Loc,[MarkD])]
    g3b = M.map $ L.sortBy (compare `on` fst)
    g4 :: Map String [(Loc,[MarkD])] -> Map String [(Loc,MarkD)]
    g4 = M.map expandTuples
    g5 :: Map String [(Loc,MarkD)] -> Map String [Utm]
    g5 = M.mapWithKey toAdjustMarks
-- computeAdjustUtms' timeSigs =
--     map adjustToUtm . absToRelTimeAdjusts . mapMaybe toAdjustMarkAbs . g2 .g1
--   where
--     g1 :: Map Loc (Map String [MarkD]) -> Map Loc [(String,MarkD)]
--     g1 = M.map (expandTuples . M.toAscList)
--     g2 :: Map Loc [(String,MarkD)] -> [(Loc,(String,MarkD))]
--     g2 = expandTuples . M.toAscList

-- toAdjustMarks :: String -> [(Loc,MarkD)] -> [Utm]
-- toAdjustMarks s ms = ("toAdjustMarks\n" ++ unlines (map show out)) `trace` out
--   where
    -- out = toAdjustMarks' s ms


toAdjustMarks :: String -> [(Loc,MarkD)] -> [Utm]
toAdjustMarks staffN = map adjustToUtm . absToRelTimeAdjusts . g1
  where
    g1 :: [(Loc,MarkD)] -> [TimeAdjustAbs]
    g1 = mapMaybe (toAdjustMarkAbs staffN)

flipLocStaffN :: [(Loc,(String,[MarkD]))] -> [(String,(Loc,[MarkD]))]
flipLocStaffN = map g
  where
    g (l,(s,ms)) = (s,(l,ms))

expandTuples :: [(a,[b])] -> [(a,b)]
expandTuples = concatMap (\(x,ys) -> map (x,) ys)

toAdjustMarkAbs :: String -> (Loc,MarkD) -> Maybe TimeAdjustAbs
toAdjustMarkAbs staffN (loc,AdjustL dur amt) = Just $ TimeAdjustAbs loc staffN LeftWarp dur amt
toAdjustMarkAbs staffN (loc,AdjustR dur amt) = Just $ TimeAdjustAbs loc staffN RightWarp dur amt
toAdjustMarkAbs _ _                          = Nothing

--     0     -1     1      2     -2
--     0     -1     

absToRelTimeAdjusts :: [TimeAdjustAbs] -> [TimeAdjustRel]
absToRelTimeAdjusts = snd . L.mapAccumL step init
  where
    init :: Double
    init = 0
    step :: Double -> TimeAdjustAbs -> (Double,TimeAdjustRel)
    step prior (TimeAdjustAbs loc staffN side dur amt) = (amt,newAdjust)
      where
        newAdjust = TimeAdjustRel loc staffN side dur (amt-prior)
    
adjustToUtm :: TimeAdjustRel -> Utm
adjustToUtm (TimeAdjustRel loc staffN side dur amt) = case side of
  LeftWarp  -> UtmLShift (Just staffN) loc dur amt
  RightWarp -> UtmRShift (Just staffN) loc dur amt

