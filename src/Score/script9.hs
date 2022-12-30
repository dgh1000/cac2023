{-# LANGUAGE TupleSections #-}

import qualified Data.Map.Strict as M
import qualified Data.List as L
import Data.Map.Strict(Map)
import Data.Maybe
import Text.Printf
import Util.Exception
import Common.CommonUtil
import Common


data MapSearchItem b = SiFound b
                     | SiNonApplicable
                     | SiError String


searchMapBw :: Ord a => String -> (a -> MapSearchItem b) -> Map Loc [a] -> Loc ->
               b
searchMapBw notFoundErr g m loc =
  case L.find isJust $ map (findFunc g) $
       concatMap expand $ M.toDescList $ fst $ M.split loc m of
    Nothing       -> throwMine notFoundErr
    Just (Just y) -> y


expand :: Ord a => (Loc,[a]) -> [(Loc,a)]
expand (loc,xs) = L.sort $ map (loc,) xs


findFunc :: (a -> MapSearchItem b) -> (Loc,a) -> Maybe b
findFunc g (loc,x) = case g x of
  SiFound y       -> Just y
  SiNonApplicable -> Nothing
  SiError s       -> throwMine $ printf "error at %s: %s" (showLoc2 loc) s


data Thing = T2 Double
           | T1
           | TErr
           deriving(Eq,Ord,Show)


globMap :: Map Loc [Thing]
globMap = M.fromList [ (Loc 1 1, [T2 4, T1])
                     , (Loc 2 1, [] ) ]


thingToSi :: Thing -> MapSearchItem Double
thingToSi (T2 d) = SiFound d
thingToSi T1     = SiNonApplicable
thingToSi TErr   = SiError "yup"


testList = [T2 1, T1, TErr, T1, T2 1]


main = do
  let x :: Double
      x = searchMapBw "not found" thingToSi globMap (Loc 2 1)
  putStrLn $ show x
  putStrLn $ show $ L.sort testList
