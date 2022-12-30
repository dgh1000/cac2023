
-- curves need idea they may control just about anything or be references for
-- any quantity including computations on other note parameters. will be
-- double mapped to double, no problem always doing that. may be undefined,
-- lookup needs to show when defined or not. when checking if in range, round
-- to 0.001

module Translation.Curves where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Text.Printf
import Data.Map.Strict(Map)
import Data.Set(Set)
import Data.Maybe
import Data.Ratio
import Common
import Common.CommonUtil
import Translation.TimeMap
import Translation
import Util.Math
import Util.Map
import Util.Showable
import System.IO.Unsafe


-- buildCurve
--
--   Constructs a OneCurve object.
--
--   Input:
--
--     list of segments, which is
--
--       begin and end Loc of each segment, locB and locE. and begin and end
--       loudness level
--
--   Output: OneCurve
--
--   Computations needed: for each pair locB and locE, they need to changed to
--   a time in seconds. Then a reverse map of times in seconds mapped back to
--   locB and locE needs to be constructed.
--

buildCurveTest :: TimeSigs -> AbsTimeMap -> [(Loc,(Loc,(Double,Double)))] ->
                  OneCurve
buildCurveTest ts atm inp =
  let
    c = buildCurve ts atm inp
  in
    unsafePerformIO $ do
      let s = showiToString $ showOneCurve c
      writeFile "me.txt" s
      return c


buildCurve :: TimeSigs -> AbsTimeMap -> [(Loc,(Loc,(Double,Double)))] ->
              OneCurve
buildCurve ts (AbsTimeMap atm) inp =
  OneCurve (M.fromList $ map f inp) ts (reverseMap atm)
  where
    f :: (Loc,(Loc,(Double,Double))) -> (Double,Seg Double)
    f (l1,(l2,(y1,y2))) = (x1, Seg x2 y1 y2)
      where
        x1 = lookupTime l1 (AbsTimeMap atm)
        x2 = lookupTime l2 (AbsTimeMap atm)

reverseMap :: (Ord k, Ord a) => Map k a -> Map a k
reverseMap = M.fromList . map (\(x,y) -> (y,x)) . M.toList


showCurve :: Curve -> ShowItem
showCurve (Curve cs) = Component "Curve" True (map showC cs)
  where
    showC (OneCurve m timeSigs timeMap) = Component "OneCurve" True
      (map showPair $ M.toAscList m)
      where
        showPair (x1,Seg x2 y1 y2) =
          SingleLine $ printf "(%s,%s) (%8.3f,%8.3f)"
            (showLoc2 $ timeToLoc x1 timeSigs timeMap)
            (showLoc2 $ timeToLoc x2 timeSigs timeMap)
            y1 y2
                   

showOneCurve :: OneCurve -> ShowItem
showOneCurve = showC
  where
    showC (OneCurve m timeSigs timeMap) = Component "OneCurve" True
      (map showPair $ M.toAscList m)
      where
        showPair (x1,Seg x2 y1 y2) =
          SingleLine $ printf "(%s,%s) (%8.3f,%8.3f)"
            (showLoc2 $ timeToLoc x1 timeSigs timeMap)
            (showLoc2 $ timeToLoc x2 timeSigs timeMap)
            y1 y2

curveLookup :: Double -> Curve -> Maybe Double
curveLookup t (Curve oneCurves) =
  case mapMaybe (oneCurveLookup t) oneCurves of
    [] -> Nothing
    xs -> Just $ sum xs


-- what don't we want to happen? like two same Locs get different answer.
-- Loc doesn't even make it onto the curve
--
-- a number that is exactly on the nose, should it be the prior seg or the 
oneCurveLookup :: Double -> OneCurve -> Maybe Double
oneCurveLookup t (OneCurve m _ _) = case M.lookupLE t m of
  Nothing -> Nothing
  Just (t1,Seg t2 v1 v2)
    | t <= t2   -> Just $ scale_3_2 t1 t t2 v1 v2
    | otherwise -> Nothing

----------------------------------------------------------------------




definedSegsOf :: Double -> OneCurve -> [(Double,Double)]
definedSegsOf tolerance (OneCurve segs _ _) =
    weld Nothing . map g $ M.toAscList segs
  where
    g (x1,Seg x2 _ _) = (x1,x2) 

tolerance = 0.005
    
weld Nothing [] = []
weld Nothing (x:xs) = weld (Just x) xs
weld (Just x) [] = [x]
weld (Just (x1,x2)) ((y1,y2):remain)
  | x2 < y1-tolerance = (x1,x2) : weld Nothing ((y1,y2):remain)
  | otherwise         = weld (Just (x1,y2)) remain
               
