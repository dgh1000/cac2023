
module Translation.GenericShape where


import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe
import Text.Printf
import Data.Map.Strict(Map)
import Data.Ratio
import Control.Monad
import Control.Monad.State
import Control.Lens
import Score.ScoreData
import Translation
import Util.Map
import Util.Exception
import Common 
import Common.CommonUtil


-- What is needed to process GenericShape marks:
--
--   - read MarkD in XML file (ParseMarks.hs)
--
--   - change all such marks to [GsCombined].
--
--       any GsCombined data includes the staff name
--
--   - the prior two steps are done by computeGsCombined
--
--   - pass every shape mark to the right 'meta-instrument'. 









-- computeGsCombined
--
-- Compute all GsCombined data for all staves.
--
-- Computing GsCombined has two steps:
--
--   First extracting just the generic shape related marks, which are
--   represented by GenericShapeMark. We put this in a double map of
--   staff name to map to loc to all GenericShapeMarks at that Loc.
--
--
computeGsCombined :: Tr [GsCombined]
computeGsCombined = do
  gsmMap <- computeGsmMap
  return $ concat $ M.mapWithKey gsCombinedStaff gsmMap


computeGsmMap :: Tr (Map String (Map Loc [GenericShapeMark]))
computeGsmMap = do
  marks <- scMarksByStaff `liftM` gets (view score)
  return $ M.map (lMapMaybeWithKey maybeGsm) marks


maybeGsm :: Loc -> MarkD -> Maybe GenericShapeMark
maybeGsm loc (GenericShape (GsLeft   t amt)) = Just $ GsmLeft   loc t amt
maybeGsm loc (GenericShape (GsRight  t))     = Just $ GsmRight  loc t
maybeGsm loc (GenericShape (GsCenter t))     = Just $ GsmCenter loc t
maybeGsm loc (GenericShape (GsOneLoc t amt)) = Just $ GsmOneLoc loc t amt
maybeGsm _ _ = Nothing

type GsMarkList = [(Loc,[GenericShapeMark])]

-- gsCombinedStaff
--
--   Convert the 'GenericShapeMarks' on one staff to a list of
--   GsCombined.
--
--   Algorithm
--
--     Input: M: map of Loc to GenericShapeMark (which is same information as
--     MarkD, just limited to generic-shape-related marks).
--
--     Find list of all Locs in M: L
--
--     Map f over each loc in L.
--
--       If L is not a center mark (!) or one loc mark (!!), do nothing.
--
--       If L is a center mark or one loc mark, this will construct two lists
--       of GenericShapeMarks:
--
--         L1: One starting before L and moving backward in time.
--
--         L2: One starting after L and moving forward in time.
--
--       It will seek a Left mark in L1 or Right mark in L2 with the same
--       shape type, seeking only as far as seekNumMsr measures away, and
--       not seeking past another center mark with same character.
--
--     It will then possibly construct a GsCombined out of the staff name, the
--     center loc, the type character, any amount specified in the center
--     mark, and maybe the left and right locs.
gsCombinedStaff :: String -> Map Loc [GenericShapeMark] -> [GsCombined]
gsCombinedStaff staffN gsMap = concatMap f locs
  where
    locs :: [Loc]
    locs = M.keys gsMap
    f :: Loc -> [GsCombined]
    f loc = mapMaybe (f2 right) ms
      where
        (left,ms,right) = f1 loc
    f1 :: Loc -> (GsMarkList,[GenericShapeMark],GsMarkList)
    f1 loc = case M.splitLookup loc gsMap of
      (m1,Just ms,m2) -> (M.toDescList m1,ms,M.toAscList m2)
    -- f2:  given a GenericShapeMark as looked up in gsMap, and 
    --      
    f2 :: GsMarkList -> GenericShapeMark -> Maybe GsCombined
    f2 ms2 (GsmLeft loc typ amt) =
      Just $ GsCombined staffN loc typ amt 
             (seekC loc typ ms2) (seekR loc typ ms2)
    f2 ms2 (GsmOneLoc loc typ amt) =
      Just $ GsCombined staffN loc typ amt Nothing Nothing
    f2 _ _ = Nothing
   

seekNumMsr = 3

{-
seekL :: Loc -> String -> GsMarkList -> Maybe Loc
seekL (Loc msrIn _) cIn ms = fmap fst $ L.find pred2 ms3 
  where
    ms2 = expand ms
    ms3 = takeWhile pred ms2
    pred (Loc m _,gsm) = case gsm of
      GsmCenter c _ | c == cIn -> False
      _                          -> m >= msrIn - seekNumMsr
    pred2 (_,GsmLeft  c) = c == cIn
    pred2 _               = False
-}

seekC :: Loc -> String -> GsMarkList -> Maybe Loc
seekC (Loc msrIn _) cIn  ms = fmap fst $ L.find pred2 ms3
  where
    ms2 = expand ms
    ms3 = takeWhile pred ms2
    -- pred is a predicate test to force search to end before encountering
    -- a 'right-type' mark of the same type
    pred (Loc m _,gsm) = case gsm of
      GsmRight _ c | c == cIn -> False
      _                       -> m <= msrIn + seekNumMsr
    pred2 (_,GsmCenter _ c) = c == cIn
    pred2 _                 = False

-- seekR
--
--   Given the loc of a g.s. midpoint, and a GsMarkList (a list of tuples
--   of loc and marks at that loc), search for Maybe Loc of a matching
--   right generic shape.
seekR :: Loc -> String -> GsMarkList -> Maybe Loc
seekR (Loc msrIn _) cIn ms = fmap fst $ L.find pred2 ms3 
  where
    -- change list of (a,[b]) to [(a,b)]
    ms2 = expand ms
    -- take while 'pred' is true - this is so that in searching for a right
    -- g.s. mark, we don't go past another center mark of same type
    -- character. We also don't want to go more than 'seekNumMsr' measures
    -- past the input center mark location
    ms3 = takeWhile pred ms2
    -- 'pred' tests if we've hit another 'left-type' or 'one-loc-type' shape
    -- mark of the same type, and stops search there
    pred (Loc m _,gsm) = case gsm of
      GsmLeft   _ c _ | c == cIn -> False
      GsmOneLoc _ c _ | c == cIn -> False
      _                          -> m <= msrIn + seekNumMsr
    -- 
    pred2 (_,GsmRight _ c) = c == cIn
    pred2 _                = False



expand :: [(a,[b])] -> [(a,b)]
expand = concatMap (\(x,ys) -> zip (repeat x) ys)



exampleToUnitTimeMod :: Map Int TimeSig -> GsCombined -> [Utm]
exampleToUnitTimeMod timeSigs c@(GsCombined _ loc typ amt locL locR) =
  case (typ,amt,locL,locR) of
    ("a",[d],Just locL2,Nothing) -> testUtmA timeSigs c
    _ -> []

{-
-- allGenericShapes
--
--   Input: one GsCombined, and list of GsFunc
--
--   - see if one of the GsFuncs can handle the GsCombined (produces output
--     for it).
--
--   Output: the output of the GsFunc that handled the GsCombined
--
--   Error condition: if no GsFunc can handle it (all GsFuncs produce
--   and empty list as output)
oneStaffGsToUtms :: String -> Map Int TimeSig -> [GsFunc] -> GsCombined ->
            [Utm] 
oneStaffGsToUtms metaName timeSigs funcs gs =
    case L.find (not . null) $ map (\f -> f timeSigs gs) funcs of
      Nothing -> throwMine $ printf ("Error in GenericShape of type '%c', " ++
                 "on staff '%s', meta instr '%s': no matching function " ++
                 "defined in instrument ") (gcType gs) (gcStaffN gs)
                 metaName 
      Just utms -> utms
-}

testUtmA :: GsFunc
testUtmA timeSigs (GsCombined _ loc "a" [amtQuars] (Just locL) Nothing) =
  [ UtmWarp Nothing locL loc UrsTowardEnd (Left (-amtQuars/2))
  , UtmPause Nothing loc amtQuars ]



toUtmErr :: Loc -> (Char,Maybe Double,Maybe Loc,Maybe Loc) -> [UnitTimeMod]
toUtmErr loc (typ,amt,locL,locR) =
  throwMine $ printf ("unrecognized generic shape with: (type: %s) " ++
                      "(amt: %s) (left loc: %s) (right loc: %s)")
              [typ] amtF locLF locRF
    where
      amtF = case amt of {Nothing -> "Nothing"; Just x -> printf "%.2f" x}
      locLF = case locL of {Nothing -> "Nothing"; Just l -> showLoc2 l}
      locRF = case locR of {Nothing -> "Nothing"; Just l -> showLoc2 l}
                           



----------------------------------------------------------------------
gsFuncErrMsg :: String -> GsCombined -> [Utm]
gsFuncErrMsg msg (GsCombined staffN loc typ _ _ _) =
  throwMine $ printf ("Error, generic shape on '%s' at %s: for type '%s', "++
                      "%s") staffN (showLoc2 loc) typ msg

----------------------------------------------------------------------
--               templates for Generic Shapes 


gsFuncCombine :: GsFunc -> GsFunc -> GsFunc
gsFuncCombine g1 g2 timeSigs c = case g1 timeSigs c of
  [] -> case g2 timeSigs c of
    [] -> throwMine $ printf ("in gsFuncCombine: the provided functions " ++
          "didn't translate the generic shape mark(s) on staff %s at " ++
          "%s") (gcStaffN c) (showLoc2 $ gcLoc c)
    xs -> xs
  xs -> xs



gsFromJust (Just x) = x


gsFunc_miniRamp :: TimeSigs -> Loc -> Rational -> Rational -> Double ->
                   [Utm]
gsFunc_miniRamp timeSigs loc diff1 diff2 t2 = 
    [ UtmRamp Nothing loc  loc1 1 t2 
    , UtmRamp Nothing loc1 loc2 t2 1 ]
  where
    loc1 = gsFromJust $ locAddQuar timeSigs loc  diff1
    loc2 = gsFromJust $ locAddQuar timeSigs loc1 diff2
