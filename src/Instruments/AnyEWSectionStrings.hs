{-# LANGUAGE TemplateHaskell , FlexibleInstances, 
    FunctionalDependencies #-}

module Instruments.AnyEWSectionStrings where

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

ew18Violins staffN destDXF destKS =
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
    qm = anySimpleModExample "in processing ew18Violins" False
                             (VelCurve [(0.99,1),(8.01,127)]) destDXF 
    df _ artic = case artic of
      "dxf" -> destDXF
      _     -> destKS
    vcf _ artic = case artic of
      "dxf"    -> defVelCurve
      _        -> defVelCurve
    ksf snote artic = simpleKSChoice snote artic 
      [(Staccato,"stac"),(Accent,"susVibHard")] "18 violins" 
      [ ("dxf"       , Nothing)
      , ("susVib"    , Just (24 + 0))
      , ("qLeg"      , Just (24 + 1))
      , ("susLeg"    , Just (24 + 2))
      , ("expLeg"    , Just (24 + 3))
      , ("lyrLeg"    , Just (24 + 4))
      , ("susLeg"    , Just (24 + 5))
      , ("nonVib"    , Just (24 + 6))
      , ("lyr"       , Just (24 + 7))
      , ("lyrFast"   , Just (24 + 8))
      , ("expFast"   , Just (24 + 9))
      , ("exp"       , Just (24 + 10))
      , ("quickUpDn" , Just (24 + 11))
      , ("martUpDn"  , Just (36 + 0))
      , ("marcLong"  , Just (36 + 1))
      , ("pizzRR"    , Just (36 + 2))
      , ("marcShort" , Just (36 + 3))
      , ("spiccatoRR", Just (36 + 4))
      , ("bartokPizz", Just (36 + 5))
      , ("clstr&Air" , Just (36 + 6))
      , ("pendereki" , Just (36 + 7))
      , ("slurFast"  , Just (36 + 8))
      , ("slurMed"   , Just (36 + 9))
      , ("slurSlow"  , Just (36 + 10))
      , ("slurSlow"  , Just (36 + 10))
      ]

ew10Cellos staffN destDXF destKS =
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
    qm = anySimpleModExample "in processing ew10Cellos" False
                             (VelCurve [(0.99,1),(8.01,127)]) destDXF 
    df _ artic = case artic of
      "dxf" -> destDXF
      _     -> destKS
    vcf _ artic = case artic of
      "dxf"    -> defVelCurve
      _        -> defVelCurve
    ksf snote artic = simpleKSChoice snote artic 
      [(Staccato,"marcRR"),(Accent,"portLeg")] "10 cellos" 
      [ ("dxf"       , Nothing)
      , ("susVib"    , Just (24 + 0))
      , ("qLeg"      , Just (24 + 1))
      , ("susLeg"    , Just (24 + 2))
      , ("portLeg"   , Just (24 + 3))
      , ("expLyrLeg" , Just (24 + 4))
      , ("trem"      , Just (24 + 5))
      , ("tr1"       , Just (24 + 6))
      , ("tr2"       , Just (24 + 7))
      , ("port"      , Just (24 + 8))
      , ("lyrFast"   , Just (24 + 9))
      , ("expVibFast", Just (24 + 10))
      , ("lyr"       , Just (84 + 4))
      , ("expVib"    , Just (84 + 5))
      , ("crec"      , Just (84 + 6))
      , ("martUpDn"  , Just (84 + 7))
      , ("marcRR"    , Just (84 + 8))
      , ("pizzRR"    , Just (84 + 9))
      , ("spiccatoRR", Just (84 + 10))
      , ("colLegnoRR", Just (84 + 11))
      , ("bartokPizz", Just (96 + 0))
      ]

