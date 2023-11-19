{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses,
    FunctionalDependencies #-}

module Instruments.AnyEWSoloStrings where

import qualified Data.Map.Strict as M
import Data.Map.Strict(Map)
import Debug.Trace
import Text.Printf
import Control.Arrow
import Control.Lens
import Control.Lens.TH
import Instruments
import System.Environment
import Score.ScoreData
-- import Translation.RunOnce
import Translation
import Translation.GenericShape
import Configs.GenericShapeMasterFunctions
import Common
import Instruments.Any
import Instruments.AnyEWUtils
import Util.Exception
import Translation.InstrUtils (updateEndAlter)

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

--- utils -----------------------------------------------------------------------------------
{-
simpleKSChoice :: SNote -> String -> String -> String -> [(String,Maybe Int)] -> Maybe Int
simpleKSChoice sNote articName stacArtic instrName otherKS
  = case lookup artic otherKS of
      Just k -> k
      Nothing -> throwMine $ "artic name " ++ articName ++ " not found in instr " ++ instrName
  where 
    artic | Staccato `elem` (cModifiers . snChord) sNote = stacArtic
          | otherwise = articName
  
-}
--- solo flute ----------------------------------------------------------------------------------------


---- solo violin ---------------------------------------------------------------------------------

ewSoloViolin staffN destDXF destKS =
  makeAnySimple staffN gsFunc1 False
  AnySimple { _anySimpleAccentAmt  = 0.6
            , _anySimpleStacFn     = return
            , _anySimpleEndAlterFn = updateEndAlter False
            , _anySimpleArpFn      = return
            , _anySimpleModFn      = qm
            , _anySimpleDestFn     = df
            , _anySimpleVelSource  = VsCurve vcf
            , _anySimpleKsFn       = ksf
            , _anySimpleModifFn    = \_ _ -> []
            , _anySimpleVolFn      = const (Left 127)
            , _anySimpleExprFn     = const (Left 127)
            , _anySimpleAliases    = [ ("ord" , "susVibHard")
                                     , ("soft", "susVibSoft") ]
            , _anySimpleStaffN     = staffN
            , _anySimpleConfigs    = M.empty
            }
  where
    qm = anySimpleModExample "in processing qViolin" False
                             (VelCurve [(0.99,1),(8.01,127)]) destDXF 
    st snote artic = standardizeChordModifers snote artic
       [(Staccato,"susVibHard")]
    df _ artic = case artic of
      "dxf" -> destDXF
      _     -> destKS
    vcf snote artic = case st snote artic of
      "dxf"    -> defVelCurve
      "susLeg" -> VelCurve [(0.99,5),(8.01,127)]
      _        -> defVelCurve
    ksf snote artic = case st snote artic of
      "dxf"          -> Nothing
      "susVibHard"   -> Just (24 + 0 )
      "nonVibSft"    -> Just (24 + 1 )
      "qLeg"         -> Just (24 + 2 )
      "susLeg"       -> Just (24 + 3 )
      "expLeg"       -> Just (24 + 4 )
      "tr1"          -> Just (24 + 5 )
      "tr2"          -> Just (24 + 6 )
      "exp1"         -> Just (24 + 7 )
      "exp2"         -> Just (24 + 8 )
      "expCrec"      -> Just (24 + 9 )
      "expP"         -> Just (24 + 10)
      "susVibSoft"   -> Just (24 + 11)
      "legVib"       -> Just (36 + 0 )
      "mart"         -> Just (36 + 1 )
      "marc"         -> Just (36 + 2 )
      "pizz"         -> Just (36 + 3 )
      "stac"         -> Just (36 + 4 )
      "colLegno"     -> Just (36 + 5 )
      "slideUp5"     -> Just (36 + 6 )
      "slideUp8"     -> Just (36 + 7 )
      "slideDn8"     -> Just (36 + 8 )
      "slur"         -> Just (36 + 9 ) 


ewSoloViola staffN destDXF destKS =
  makeAnySimple staffN gsFunc1 False
  AnySimple { _anySimpleAccentAmt  = 0.6
            , _anySimpleStacFn     = return
            , _anySimpleEndAlterFn = updateEndAlter False
            , _anySimpleArpFn      = return
            , _anySimpleModFn      = qm
            , _anySimpleDestFn     = df
            , _anySimpleVelSource  = VsCurve vcf
            , _anySimpleKsFn       = ksf
            , _anySimpleModifFn    = \_ _ -> []
            , _anySimpleVolFn      = const (Left 127)
            , _anySimpleExprFn     = const (Left 127)
            , _anySimpleAliases    = [ ]
            , _anySimpleStaffN     = staffN
            , _anySimpleConfigs    = M.empty
            }
  where
    qm = anySimpleModExample "in processing ewSoloViola" False
                             (VelCurve [(0.99,1),(8.01,127)]) destDXF 
    st snote artic = standardizeChordModifers snote artic
       [(Staccato,"marcHardRR"),(Accent,"marcHardRR")]
    df _ artic = case artic of
      "dxf" -> destDXF
      _     -> destKS
    vcf snote artic = case st snote artic of
      "dxf"    -> defVelCurve
      "susLeg" -> VelCurve [(0.99,5),(8.01,127)]
      _        -> defVelCurve
    ksf snote artic = case st snote artic of
      "dxf"        -> Nothing
      "susVib"     -> Just (24 + 0)
      "nonVibRR"   -> Just (24 + 1)
      "qLeg"       -> Just (24 + 2)
      "susLeg"     -> Just (24 + 3)
      "expLeg"     -> Just (24 + 4)
      "tr1"        -> Just (24 + 5)
      "tr2"        -> Just (24 + 6)
      "exp1"       -> Just (24 + 7)
      "exp2"       -> Just (24 + 8)
      "exp3"       -> Just (24 + 9)
      "expVibSft"  -> Just (24 + 10)
      "martRR"     -> Just (24 + 11)
      "marcHardRR" -> Just (36 + 0)
      "pizzRR"     -> Just (36 + 1)
      "spicRR"     -> Just (36 + 2)
      "colLegnoRR" -> Just (36 + 3)
      "8vaSlideUp" -> Just (36 + 4)

ewSoloCello staffN destDXF destKS =
  makeAnySimple staffN gsFunc1 False
  AnySimple { _anySimpleAccentAmt  = 0.6
            , _anySimpleStacFn     = return
            , _anySimpleEndAlterFn = updateEndAlter False
            , _anySimpleArpFn      = return
            , _anySimpleModFn      = qm
            , _anySimpleDestFn     = df
            , _anySimpleVelSource  = VsCurve vcf
            , _anySimpleKsFn       = ksf
            , _anySimpleModifFn    = \_ _ -> []
            , _anySimpleVolFn      = const (Left 127)
            , _anySimpleExprFn     = const (Left 127)
            , _anySimpleAliases    = []
            , _anySimpleStaffN     = staffN
            , _anySimpleConfigs    = M.empty
            }
  where
    qm = anySimpleModExample "in processing qSoloCello" False
                             (VelCurve [(0.99,1),(8.01,127)]) destDXF 
    st snote artic = standardizeChordModifers snote artic
       [(Staccato,"marc"),(Accent,"susVibHard")]
    df _ artic = case artic of
      "dxf" -> destDXF
      _     -> destKS
    vcf snote artic = case st snote artic of
      "dxf"    -> defVelCurve
      _        -> defVelCurve
    ksf snote artic = case st snote artic of
      "dxf"             -> Nothing
      "susVibSmooth"    -> Just $ 24 + 0
      "qLeg"            -> Just $ 24 + 1
      "susLeg"          -> Just $ 24 + 2
      "nonVib"          -> Just $ 24 + 3
      "expDn"           -> Just $ 24 + 4
      "expUp"           -> Just $ 24 + 5
      "doubleBowExp"    -> Just $ 24 + 6
      "expVib"          -> Just $ 24 + 7
      "martUpDn"        -> Just $ 84 + 4
      "marc"            -> Just $ 84 + 5
      "pizzRR"          -> Just $ 84 + 6
      "colLegnoRR"      -> Just $ 84 + 7

