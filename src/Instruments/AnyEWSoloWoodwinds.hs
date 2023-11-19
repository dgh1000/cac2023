{-# LANGUAGE TemplateHaskell, FlexibleInstances, FunctionalDependencies #-}

module Instruments.AnyEWSoloWoodwinds where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
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

--- solo flute ----------------------------------------------------------------------------------------


ewSoloFlute staffN destDXF destKS =
  makeAnySimple staffN gsFunc1 False
  AnySimple { _anySimpleAccentAmt  = 0.6
            , _anySimpleStacFn     = return
            , _anySimpleEndAlterFn = updateEndAlter False
            , _anySimpleArpFn      = return
            {-
            _anySimpleModFn        :: String -> Loc -> Loc -> Tr [TrRaw] -- staffN
            anySimpleModExample :: String -> Bool -> VelCurve -> (Int,Int) ->
                       String -> Loc -> Loc -> Tr [TrRaw]
            -}
            , _anySimpleModFn      = qm
            , _anySimpleDestFn     = df
            , _anySimpleVelSource  = VsCurve vcf
            , _anySimpleKsFn       = ksf
            , _anySimpleModifFn    = \_ _ -> []
            , _anySimpleVolFn      = const (Left 127)
            , _anySimpleExprFn     = const (Left 127)
            {-
            , _anySimpleAliases    = [("ord", "susVib")
                                     ,("exp", "expLeg")]
            -}
            , _anySimpleAliases    = []
            , _anySimpleStaffN     = staffN
            , _anySimpleConfigs    = M.empty
            }
  where
    qm = anySimpleModExample "in processing ewSoloFlute" False
                             (VelCurve [(0.99,1),(8.01,127)]) destDXF
    df snote artic 
      | Staccato `elem` (cModifiers . snChord) snote = destKS
      | otherwise = case artic of
        "dxf" -> destDXF 
        _     -> destKS
    vcf _ artic = defVelCurve
      {-
      case artic of
      
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
      -}
    ksf snote artic = simpleKSChoice snote artic 
        [(Staccato,"stac")] 
        "solo flute" 
        [ ("dxf", Nothing)
        , ("susVib"      , Just (24 + 0))
        , ("qLeg"        , Just (24 + 1))
        , ("susLeg"      , Just (24 + 2))
        , ("lyrLeg"      , Just (24 + 3))
        , ("expLeg"      , Just (24 + 4))
        , ("flutterMod"  , Just (24 + 5))
        , ("tr1"         , Just (24 + 6))
        , ("tr2"         , Just (24 + 7))
        , ("sfzVib"      , Just (24 + 8))
        , ("lyr"         , Just (24 + 9))
        , ("slowExp"     , Just (24 + 10))
        , ("slowExp2"    , Just (24 + 11))
        , ("shortStac"   , Just (36 + 0))
        , ("stac"        , Just (36 + 1))
        , ("8vaRunDn"    , Just (36 + 2))
        , ("8vaRunUpDn"  , Just (36 + 3))
        , ("8vaRunUp"    , Just (36 + 4))
        , ("fall"        , Just (36 + 5))
        , ("grace"       , Just (36 + 6))
        , ("psychoRunDnRR", Just (36 + 7))
        ]

---- solo clarinet ---------------------------------------------------------------------------------

ewSoloClarinet staffN destDXF destKS =
  makeAnySimple staffN gsFunc1 False
  AnySimple { _anySimpleAccentAmt  = 0.6
            , _anySimpleStacFn     = return
            , _anySimpleEndAlterFn = updateEndAlter False
            , _anySimpleArpFn      = return
            {-
            _anySimpleModFn        :: String -> Loc -> Loc -> Tr [TrRaw] -- staffN
            anySimpleModExample :: String -> Bool -> VelCurve -> (Int,Int) ->
                       String -> Loc -> Loc -> Tr [TrRaw]
            -}
            , _anySimpleModFn      = qm
            , _anySimpleDestFn     = df
            , _anySimpleVelSource  = VsCurve vcf
            , _anySimpleKsFn       = ksf
            , _anySimpleModifFn    = mf
            , _anySimpleVolFn      = const (Left 127)
            , _anySimpleExprFn     = const (Left 127)
            {-
            , _anySimpleAliases    = [("ord", "susVib")
                                     ,("exp", "expLeg")]
            -}
            , _anySimpleAliases    = []
            , _anySimpleStaffN     = staffN
            , _anySimpleConfigs    = M.empty
            }
  where
    qm = anySimpleModExample "in processing ewClarinet" False
                             (VelCurve [(0.99,1),(8.01,127)]) destDXF
    df snote artic 
      | Staccato `elem` (cModifiers . snChord) snote = destKS
      | otherwise = case artic of
        "dxf" -> destDXF 
        _     -> destKS
    vcf _ artic = defVelCurve
      {-
      case artic of
      
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
      -}
    st snote artic = standardizeChordModifers snote artic
      [(Staccato,"stac")]
    mf s artic = case st s artic of
      "stac" -> legatoModif False
      _      -> legatoModif True
    ksf snote artic = simpleKSChoice snote artic 
        [(Staccato,"stac")] 
        "solo clarinet" 
        [ ("dxf", Nothing)
        , ("nonVib"       , Just (24 + 0))
        , ("qLeg"         , Just (24 + 1))
        , ("susLeg"       , Just (24 + 2))
        , ("portLeg"      , Just (24 + 3))
        , ("expLeg"       , Just (24 + 4))
        , ("tr1"          , Just (24 + 5))
        , ("tr2"          , Just (24 + 6))
        , ("portato"      , Just (24 + 7))
        , ("expFast"      , Just (24 + 8))
        , ("expSlowCresc" , Just (24 + 9))
        , ("marc"     , Just (24 + 10))
        , ("stac"    , Just (24 + 11))
        , ("8vaRunDn"   , Just (36 + 0))
        , ("8vaRunUpDn"        , Just (36 + 1))
        , ("8vaRunUp"    , Just (36 + 2))
        , ("graceNotes"  , Just (36 + 3))
        ]
