{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses,
    FunctionalDependencies #-}

module Instruments.AnyQ where

import qualified Data.Map.Strict as M
import Data.Map.Strict(Map)
import Debug.Trace
import Text.Printf
import Control.Arrow
import Control.Lens
import Control.Lens.TH
import Instruments
import System.Environment
import Translation.RunOnce
import Translation.GenericShape
import Configs.GenericShapeMasterFunctions
import Common
import Instruments.Any
import Util.Exception

----------------------------------------------------------------------
--         CALIBRATION EFFORTS -- December 2018

-- flute and violin
--
--   #susLeg at pp and ff match out of the box
--

-- vln   fader: 0 dB
-- flute fader: -2 dB

--
----------------------------------------------------------------------

type CalibrationSet = Map String Calibration

data Calibration = Calibration
  { _calibrationVelCurveFn  :: String -> VelCurve
  , _calibrationVolFn       :: String -> Either Int VelCurve
  }

makeFields ''Calibration


-- defaultVelCurve = VelCurve [(0,1),(9,127)]

fromTable :: String -> [(String,a)] -> String -> a
fromTable msg table artic = case lookup artic table of
  Nothing -> error $ printf "In %s, artic '%s' not found in table" msg artic
  Just x  -> x

{-
qDefault :: String -> (Int,Int) -> (AnySimple -> AnySimple) -> MetaInstr
qDefault staffN dest setter =
  makeAnySimple staffN gsFunc1 0.6 (setter anySimp) 
  where
    anySimp = setter $
      AnySimple { 0.6 return return return (\defaultVelCurve dest (const dest)
              (const defaultVelCurve) (const Nothing) (const 127)
-}


defVelCurve = VelCurve [(0.99,1),(8.01,127)]


qFlute staffN destDXF destKS =
  makeAnySimple staffN gsFunc1
  AnySimple { _anySimpleAccentAmt  = 0.6
            , _anySimpleStacFn     = return
            , _anySimpleEndAlterFn = return
            , _anySimpleArpFn      = return
            {-
  , _anySimpleModFn        :: String -> Loc -> Loc -> Tr [TrRaw] -- staffN, b/e
anySimpleModExample :: String -> Bool -> VelCurve -> (Int,Int) ->
                       String -> Loc -> Loc -> Tr [TrRaw]
-}
            , _anySimpleModFn      = qm
            , _anySimpleDestFn     = df
            , _anySimpleVelSource  = VsCurve vcf
            , _anySimpleKsFn       = ksf
            , _anySimpleVolFn      = const (Left 127)
            , _anySimpleExprFn     = const (Left 127)
            , _anySimpleAliases    = [("ord", "susVib")
                                     ,("exp", "expLeg")]
            , _anySimpleStaffN     = staffN
            }
  where
    qm = anySimpleModExample "in processing qFlute" False
                             (VelCurve [(0.99,1),(8.01,80)]) destDXF
    df _ artic = case artic of
      "dxf" -> destDXF 
      _     -> destKS
    vcf artic = case artic of
      "dxf"    -> VelCurve [(0,63),(9,64)]
      "stac"   -> VelCurve [(0.95,30) ,(8.05,110)]
      "susLeg" -> VelCurve [ (0.95,30)
                           , (2   ,60)
                           , (3   ,82)
                           , (4   ,88)
                           , (5   ,92)
                           , (7   ,115)
                           , (8.05,120)] 
      _        -> defVelCurve
    ksf _ artic = case artic of
      "dxf"    -> Nothing
      "susVib" -> Just (24 + 0)
      "susLeg" -> Just (24 + 2)
      "expLeg" -> Just (24 + 4)
      "stac"   -> Just (36 + 1)
      "tr1"    -> Just (24 + 6)
      "tr2"    -> Just (24 + 7)
      s        -> throwMine $ printf "unknown artic '%s', qFlute" s

qViolin staffN destDXF destKS =
  makeAnySimple staffN gsFunc1
  AnySimple { _anySimpleAccentAmt  = 0.6
            , _anySimpleStacFn     = return
            , _anySimpleEndAlterFn = return
            , _anySimpleArpFn      = return
            , _anySimpleModFn      = qm
            , _anySimpleDestFn     = df
            , _anySimpleVelSource  = VsCurve vcf
            , _anySimpleKsFn       = ksf
            , _anySimpleVolFn      = const (Left 127)
            , _anySimpleExprFn     = const (Left 127)
            , _anySimpleAliases    = [ ("ord" , "susVibHard")
                                     , ("soft", "susVibSoft") ]
            , _anySimpleStaffN     = staffN
            }
  where
    qm = anySimpleModExample "in processing qViolin" False
                             (VelCurve [(0.99,1),(8.01,127)]) destDXF 
    df _ artic = case artic of
      "dxf" -> destDXF
      _     -> destKS
    vcf artic = case artic of
      "dxf"    -> defVelCurve
      "susLeg" -> VelCurve [(0.99,5),(8.01,127)]
      _        -> defVelCurve
    ksf _ artic = case artic of
      "dxf"        -> Nothing
      "susVibHard" -> Just (24 + 0)
      "nonVibSft"  -> Just (24 + 1)
      "qLeg"       -> Just (24 + 2)
      "susLeg"     -> Just (24 + 3)
      "expLeg"     -> Just (24 + 4)
      "tr1"        -> Just (24 + 5)
      "tr2"        -> Just (24 + 6)
      "exp1"       -> Just (24 + 7)
      "exp2"       -> Just (24 + 8)
      "expCrec"    -> Just (24 + 9)
      "expP"       -> Just (24 + 10)
      "susVibSoft" -> Just (24 + 11)
      "legVib"     -> Just (36 + 0)
      "mart"       -> Just (36 + 1)
      "marc"       -> Just (36 + 2)
      "pizz"       -> Just (36 + 3)
      "stac"       -> Just (36 + 4)
      "colLegno"   -> Just (36 + 5)
      "slideUp5"   -> Just (36 + 6)
      "slideUp8"   -> Just (36 + 7)
      "slideDn8"   -> Just (36 + 8)
      "slur"       -> Just (36 + 9)


                            
            
  
