{-# LANGUAGE TypeSynonymInstances,FlexibleInstances #-}

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Control.Monad
import Text.Printf
import Text.Parsec
import Text.Parsec.String
import Score.ParseMarks
import Score.ScoreData
import Common
import Common.CommonUtil
import Util.Parse
import Util.Map
import Util.Showable
import Data.Maybe
import Data.Map(Map)
import Data.Ratio

timeSigs_1 = M.fromList
  [ (1, TimeSig 4 4)
  , (2, TimeSig 4 4) 
  , (3, TimeSig 4 4)
  , (4, TimeSig 4 4)
  , (5, TimeSig 4 4)
  , (6, TimeSig 4 4)
  , (7, TimeSig 4 4)
  , (8, TimeSig 4 4)
  , (9, TimeSig 4 4)
  ]


data_1 = listToLMap 
  [ (Loc 1 1, "a=100"    )  
  , (Loc 1 (1+1%2), "w"   )
  , (Loc 1 (2+1%2), "b=2" )
  , (Loc 2 (4+1%2), "w"   )
  , (Loc 3 1, "T=100"    )
  , (Loc 3 2, "T=3:2a"    )
  , (Loc 3 3, "T=3:2"     )
  , (Loc 4 1, "arp@5"     )
  , (Loc 4 2, "stac@5")
  , (Loc 4 3, "ps@5")
  , (Loc 4 4, "ps:15")
  , (Loc 4 (4+1%2), "ps1:4")
  , (Loc 5 1, "a=100")
  , (Loc 5 2, "a=3:2b")
  , (Loc 5 3, "rit.")
  , (Loc 5 4, "accel.")
  , (Loc 8 1, "=2:3|4:5=")
  , (Loc 8 2, "=4:5")
  , (Loc 8 3, "=4:5")
  ]


data_2 :: Map Loc [WordDirection]
data_2 = listToLMap
  [ (Loc 1 1, WdAbove "a=100") ]

{-

okay we ran into problem using our percent scheme, numbers like 50 are perfect
for beats per minute

-}


data OneLoc = OneLoc (Loc,Map String [MarkD])

instance ShowItemClass OneLoc where
  showI (OneLoc (loc,m)) =
      Component (showLoc2 loc) True (map showI $ M.toAscList m)


type M1 = (String,[MarkD])

instance ShowItemClass M1 where
  showI (name,ms) = Component name True (map (SingleLine . show) ms)


test :: Map Loc [WordDirection] -> IO ()
test words = do
  let marks  = computeWordMarks $ M.fromList [("foo",words)] 
      -- output = concatMap doLoc $ M.toAscList marks
      -- doLoc :: (Loc,[Mark]) -> String
      -- doLoc (loc,ms) = printf "%s\n" (showLoc2 loc) ++ concatMap doMark ms
      -- doMark m = printf "    %s\n" (show m)
  putStrLn . showiToString $
    Component "" False (map (showI . OneLoc) $ M.toAscList marks) 


main = test data_1
    

main2 :: Double
main2 = read ".5"
