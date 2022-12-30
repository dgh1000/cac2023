{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Instruments.TestToUnitTimeMods where

import qualified Data.Map.Strict as M
import Data.Map.Strict(Map)
import Instruments
import Instruments.InstrumentsData
import Instruments.ToUnitTimeMods
import Common
import Common.CommonUtil
import Score.ScoreData
import Util.Map
import Util.Showable


type Mark3OneLoc = Map String [Mark3]

type Mark3Map = Map Loc Mark3OneLoc

type TempoMarkOneLoc = Map String [TempoMark]

type TempoMarkMap = Map Loc TempoMarkOneLoc

instance ShowItemClass TempoMarkOneLoc where
  showI = Component "" True . map f . M.toAscList
    where
      f (name,ms) = Component name True (map g ms)
      g m = SingleLine $ show m
      
instance ShowItemClass TempoMarkMap where
  showI = Component "" True . map g . M.toAscList
    where
      g :: (Loc,TempoMarkOneLoc) -> ShowItem
      g (l,m) = let Component _ _ items = showI m
                in Component (showLoc2 l) True items



d1 :: Mark3OneLoc
d1 = M.fromList [ ("foo", [SymbolMark3 "foo" 3,Trunc3 1])
                , ("bar", [SetTempo3 100] )
                ]

d2 :: Mark3OneLoc
d2 = M.fromList [ ("foo", [RampBeg3 3])
                , ("bar", [RampEnd3 2])
                ]

d3 :: Mark3OneLoc
d3 = M.fromList [ ("foo", [Trunc3  1] )
                , ("bar", [LTrunc3 1] )
                ]


e1 = M.fromList [ (Loc 1 1, d1)
                , (Loc 1 2, d2)
                ]



main = do
  putStrLn $ showIString $ computeTempoMarks e1

  
