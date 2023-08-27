
module Translation.ToUnitTimeMods2 where


-- boundaries/adjusts: how do I know which direction to ramp? 1 on the side of
-- smaller absolute adjust?



import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Data.List as L
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
computeTmMarks m = M.mapMaybeWithKey g m
  where
    g loc m1 | M.null x  = Nothing
             | otherwise = Just x
      where x = lMapMaybe (f loc) m1
    f :: Loc -> MarkD -> Maybe TimeModMark
    f l (AbsWarp ws amt) = Just $ TmmAbsWarp ws amt
    f l (Pause amt)      = Just $ TmmPause amt
    f l W                = Just TmmW  
    f l (Boundary2 mAmt)     = Just $ TmmBoundary mAmt
    f l (Adjust2 flag _ amt) = Just $ TmmAdjust flag amt
    f l (PostPause amt)   = Just $ TmmAfterPause amt
    f l (LeftSideShift amt) = Just $ TmmShift LeftWarp amt
    f l (RightSideShift amt) = Just $ TmmShift RightWarp amt
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

toUtm1 :: Loc -> String -> TimeModMark -> [Utm]
toUtm1 loc staffN t@(TmmShift side amt) = printf "%s: %s %s" (showLoc2 loc) staffN (show t) `trace`
  toUtm1' loc staffN t
toUtm1 a b c = toUtm1' a b c

toUtm1' :: Loc -> String -> TimeModMark -> [Utm]
toUtm1' loc staffN (TmmAbsWarp _ _) = 
  throwMine $ printf "%s: absolute warp not implemented" (showLoc2 loc)
toUtm1' loc staffN (TmmPause amt) = [UtmPause Nothing loc amt]
toUtm1' loc staffN (TmmAfterPause amt) = [UtmPostPause Nothing loc amt]
toUtm1' loc staffN (TmmShift side amt) = case side of
  LeftWarp -> [UtmLShift (Just staffN) loc amt]
  RightWarp -> [UtmRShift (Just staffN) loc amt]
toUtm1' _ staffN TmmW = []
toUtm1' loc staffN (TmmBoundary _) =
  throwMine $ printf "%s: boundaries/adjusts not implemented" (showLoc2 loc)
toUtm1' loc staffN (TmmAdjust _ _) =
  throwMine $ printf "%s: boundaries/adjusts not implemented" (showLoc2 loc)


-- utmRank
--
--   Assign an integer to each type of Unit time mod, such that lower
--   interger means it should be applied first, and higher integer means
--   last.
--
--   Ramp 
utmRank :: Utm -> Int
utmRank (UtmRamp _ _ _ _ _)         = -1
utmRank (UtmWarp _ _ _ _ (Left _))  = 0
utmRank (UtmPause _ _ _)            = 1
utmRank (UtmPostPause _ _ _)        = 2
utmRank (UtmLShift _ _ _)           = 3
utmRank (UtmRShift _ _ _)           = 4
utmRank (UtmWarp _ _ _ _ (Right _)) = 5

  
