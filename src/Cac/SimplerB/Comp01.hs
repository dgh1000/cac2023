{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses,
             FunctionalDependencies #-}

module Cac.SimplerB.Comp01 where

import qualified Data.Map as M
import Text.Printf
import Control.Lens
import Control.Lens.TH
import Data.Map(Map)
import Data.Set(Set)
import Cac.Pcs 
import Cac.SimplerB.Search
import Cac.SimplerB.SimplerBData
import Cac.SimplerB.Percentile
import Util.Showable



data Note = Note
  { _noteTOn   :: Double
  , _noteTOff  :: Double
  , _notePitch :: Int
  , _noteLoud  :: Double
  }
  deriving(Show)


makeFields ''Note


data Comp01Config = Comp01Config
  { _comp01ConfigPcSubsets :: Map Int (Set Pcs)
  , _comp01ConfigValues    :: Map Pcs Double
  }

makeFields ''Comp01Config

data Comp01 = Comp01
  { _comp01Notes :: [Note]   -- in reverse order
  , _comp01LastT :: Double
  , _comp01RecentPcs :: Pcs
  , _comp01NextPitch :: Maybe Int
  , _comp01NextSpan  :: Maybe Double
  , _comp01NextDur   :: Maybe Double
  , _comp01Config    :: Comp01Config
  , _comp01EvalUnits :: [Comp01 -> EvalUnitScore]
  }

makeFields ''Comp01


type EvalUnit01 = Comp01 -> EvalUnitScore

myShowComp01 :: Comp01 -> String
myShowComp01 comp = printf "nextPitch:%s nextSpan:%s nextDur: %s"
                    (show $ view nextPitch comp) (show $ view nextSpan comp)
                    (show $ view nextDur comp)


evalComp01 :: Comp01 -> EvalUnit01 -> EvalUnitScore
evalComp01 c f = f c


data Step01 = Step01Pitch Int
            | Step01Time  Double
            | Step01Dur   Double
            | Step01Loud  Double
            deriving(Eq,Ord,Show)


data StepCase = ScPitch
              | ScTime
              | ScDur
              | ScLoud
              deriving(Eq)


addStep01 :: Comp01 -> Step01 -> Comp01
addStep01 c stepIn =
  case (getNextStepCase c,stepIn) of
    (ScPitch,Step01Pitch pit) -> set nextPitch (Just pit) c
    (ScTime ,Step01Time    t) -> set nextSpan  (Just t)   c
    (ScDur  ,Step01Dur     d) -> set nextDur   (Just d)   c
    (ScLoud ,Step01Loud    l) ->
      let (Comp01 _ lastTIn _ (Just p) (Just s) (Just d) _ _) = c  
          newT = lastTIn+s
          n = Note newT (newT+d) p l
      in set lastT newT .
         set nextPitch Nothing .
         set nextSpan Nothing .
         set nextDur Nothing .
         over notes (n:) $ c


getNextStepCase :: Comp01 -> StepCase
getNextStepCase c = case (view nextPitch c,view nextSpan c, view nextDur c) of
  (Nothing,Nothing,Nothing) -> ScPitch
  (Just _ ,Nothing,Nothing) -> ScTime
  (Just _ ,Just _ ,Nothing) -> ScDur
  (Just _ ,Just _ ,Just _ ) -> ScLoud


getCurrentStepCase :: Comp01 -> StepCase
getCurrentStepCase c = case getNextStepCase c of
  ScPitch -> ScLoud
  ScTime  -> ScPitch
  ScDur   -> ScTime
  ScLoud  -> ScDur
  

genSteps01 :: Comp01 -> [Step01]
genSteps01 comp = case getNextStepCase comp of
  ScPitch -> map Step01Pitch [20..90]
  ScTime  -> map Step01Time [0.1,0.2..2.0]
  ScDur   -> map Step01Dur  [0.1,0.2,0.3,0.5,0.7,1.0,1.3,1.8,2.3,3.0,4.0]
  ScLoud  -> map Step01Loud [2.0,2.5..8.0]


----------------------------------------------------------------------
--                     show

instance ShowItemClass Comp01 where
  showI comp01 = Component "Comp01" True [ theNotes
                                         , theNextPitch
                                         , theNextSpan
                                         , theNextDur ]
    where
      noteSingle n = SingleLine $ printf "%d" (view pitch n)
      theNotes = Component "Notes" True (map noteSingle $ view notes comp01)
      theNextPitch = case view nextPitch comp01 of
        Nothing -> SingleLine "nextPitch: Nothing"
        Just i  -> SingleLine $ printf "nextPitch: %d" i
      theNextSpan  = case view nextSpan comp01 of
        Nothing -> SingleLine "nextSpan: Nothing"
        Just d  -> SingleLine $ printf "nextSpan: %.3f" d
      theNextDur   = case view nextDur comp01 of
        Nothing -> SingleLine "nextDur: Nothing"
        Just d  -> SingleLine $ printf "nextSpan: %.3f" d


instance ShowItemClass (StepReport Comp01 Step01) where
  showI sr = Component "Step report" True
    [ sStepScores
    , sChosen
    , sEvalUnitPerc
    , SingleLine $ printf "num non-pruned: %d" (view nNonPruned sr)
    , SingleLine $ printf "num least bad half: %d" (view nLeastBadHalf sr)
    , SingleLine $ printf "num tied: %d" (view nTied sr)
    ]
    where
      sStepScores = case showI (view stepScores sr) of
        Component s _ ss -> Component ("step scores: " ++ s) True ss
      sChosen = Component "chosen step:" True [showI $ view theChosen sr] 
      sEvalUnitPerc = Component "Perc., chosen step:" True
        (map (\(s,d) -> SingleLine $ printf "name: %s perc: %.3f")
          $ M.toAscList (view evalUnitPerc sr))




instance ShowItemClass a =>  ShowItemClass (PercentileOut a) where
  showI (PercentileOut _ sing) = Component "PercentileOut singles" True
     (map showData sing)
     where
       showData (d,value) = Component (printf "%.3f" d) True [showI value]
  

instance ShowItemClass (EvalStepScore Comp01 Step01) where
  showI ess = Component "EvalStepScore" True
    [ sStep , sUnitScores , sThePruned , sSummedResult ]
    where
      sStep = SingleLine $ show (view theStep ess)
      sUnitScores = error "foo"
      sThePruned = error "foo"
      sSummedResult = error "Foo"


