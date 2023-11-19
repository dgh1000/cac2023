module Instruments.AnyEWUtils where

import Text.Printf
import Data.Set(Set)
import qualified Data.Set as S
import qualified Data.List as L
import Data.Maybe
import Score.ScoreData
import Translation
import Util.Exception
import Debug.Trace

--- utils -----------------------------------------------------------------------------------
simpleKSChoice :: SNote -> String -> [(ChordModifier,String)] -> String -> [(String,Maybe Int)] -> Maybe Int
simpleKSChoice sNote articName specialArtics instrName otherKS = 
    case lookup theArtic otherKS of
      Nothing -> throwMine $ printf "artic name %s not found in instr %s" theArtic instrName
      Just ks -> ks
  where
    int = map fst specialArtics `L.intersect` (S.toAscList . cModifiers . snChord) sNote
    theArtic :: String
    theArtic = case listToMaybe int of
      Nothing -> articName
      Just s  -> fromMaybe1 $ lookup s specialArtics

{-
simpleModifChoice :: SNote -> String -> [Modif]
simpleModifChoice _ artic = case artic of
  "stac" -> [ModifCtrl (Left $ -0.05) 68 0]
  _      -> [ModifCtrl (Left $ -0.05) 68 127]
-}

legatoModif :: Bool -> [Modif]
legatoModif True = [ModifCtrl (Left $ -0.05) 68 127]
legatoModif False = [ModifCtrl (Left $ -0.05) 68 0]


standardizeChordModifers :: SNote -> String -> [(ChordModifier,String)] -> String
standardizeChordModifers snote articIn modifierMap = fromMaybe articIn result
  where
    mods :: Set ChordModifier
    mods = cModifiers . snChord $ snote
    step :: Maybe String -> (ChordModifier,String) -> Maybe String
    step (Just a) _ = Just a
    step Nothing (mod,a) | S.member mod mods = Just a
                         | otherwise         = Nothing
    result = foldl step Nothing modifierMap


fromMaybe1 :: Maybe a -> a
fromMaybe1 x = case x of
  Just y -> y

{-
  = case lookup artic otherKS of
      Just k -> k
      Nothing -> throwMine $ "artic name " ++ articName ++ " not found in instr " ++ instrName
  where 
    artic | Staccato `elem` (cModifiers . snChord) sNote = stacArtic
          | otherwise = articName
-}

