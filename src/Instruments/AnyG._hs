{-# LANGUAGE TemplateHaskell, FlexibleInstances, MultiParamTypeClasses #-}

module Instruments.AnyG where

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


-- fromTable
--
--   Look up an articulation in a polymorphic table.
--
--   The specific types could be keyswitch or channel or both.
--
fromTable :: String -> [(String,a)] -> String -> a
fromTable msg table artic = case lookup artic table of
  Nothing -> error $ printf "In %s, artic '%s' not found in table" msg artic
  Just x  -> x


defVelCurve = VelCurve [(0.99,1),(8.01,127)]


gFlute staffN destKS =
  makeAnySimple staffN gsFunc1
  AnySimple { _anySimpleAccentAmt  = 0.75
            , _anySimpleStacFn     = return
            , _anySimpleEndAlterFn = return
            , _anySimpleArpFn      = return
            , _anySimpleModFn      = qm
            , _anySimpleDestFn     = df
            , _anySimpleVelSource  = VsArticulation
            , _anySimpleKsFn       = ksf
            , _anySimpleVolFn      = const (Left 127)
            , _anySimpleExprFn     = const (Left 127)
            , _anySimpleAliases    = [("ord", "vib")
                                     ,("exp", "vib")]
            , _anySimpleStaffN     = staffN
            }
  where
    qm = anySimpleModExample "in processing gFlute" False
                             defVelCurve destKS
    df artic = case artic of
      _ -> destKS
    {-
    vcf artic = case artic of
      -- _        -> VelCurve [(0.99,5), (9.01,127)]
      _        -> VelCurve [(0,64),(10,64)]
    -}
    ksf artic = case artic of
      "vib"     -> Just 48
      "nonVib"  -> Just 49
      "flutter" -> Just 50
      s        -> throwMine $ printf "unknown artic '%s', gFlute" s



gViolin staffN destKS =
  makeAnySimple staffN gsFunc1
  AnySimple { _anySimpleAccentAmt  = 0.75
            , _anySimpleStacFn     = return
            , _anySimpleEndAlterFn = return
            , _anySimpleArpFn      = return
            , _anySimpleModFn      = qm
            , _anySimpleDestFn     = df
            , _anySimpleVelSource  = VsArticulation
            , _anySimpleKsFn       = ksf
            , _anySimpleVolFn      = const (Left 127)
            , _anySimpleExprFn     = const (Left 127)
            , _anySimpleAliases    = [("ord", "sustain")]
            , _anySimpleStaffN     = staffN
            }
  where
    qm = anySimpleModExample "in processing gFlute" False
                             defVelCurve destKS
    df artic = case artic of
      _ -> destKS
    {-
    vcf artic = case artic of
      -- _        -> VelCurve [(0.99,5), (9.01,127)]
      _        -> VelCurve [(0,64),(10,64)]
    -}
    ksf artic = case artic of
      "sustain"        -> Just 36
      "sustainMute"    -> Just 37
      "autoAlternate"  -> Just 38
      "upbows"         -> Just 39
      "downbows"       -> Just 40
      "pizz"           -> Just 41
      "tremoloMute"    -> Just 42
      "tremolo"        -> Just 43
      "halfTrillMute"  -> Just 44
      "halfTrill"      -> Just 45
      "wholeTrillMute" -> Just 46
      "wholeTrill"     -> Just 47
      s        -> throwMine $ printf "unknown artic '%s', gViolin" s


{-

qViolin staffN destDXF destKS =
  makeAnySimple staffN gsFunc1
  AnySimple { _anySimpleAccentAmt  = 0.6
            , _anySimpleStacFn     = return
            , _anySimpleEndAlterFn = return
            , _anySimpleArpFn      = return
            , _anySimpleModFn      = qm
            , _anySimpleDestFn     = df
            , _anySimpleVelCurveFn = vcf
            , _anySimpleKsFn       = ksf
            , _anySimpleVolFn      = const (Left 127)
            , _anySimpleExprFn     = const (Left 127)
            , _anySimpleAliases    = [ ("ord" , "susVibHard")
                                     , ("soft", "susVibSoft") ]
            , _anySimpleStaffN     = staffN
            }
  where
    qm = anySimpleModExample "in processing qViolin"
                             (VelCurve [(0.99,1),(8.01,127)]) destDXF 
    df artic = case artic of
      "dxf" -> destDXF
      _     -> destKS
    vcf artic = case artic of
      "dxf"    -> defVelCurve
      "susLeg" -> VelCurve [(0.99,5),(8.01,127)]
      _        -> defVelCurve
    ksf artic = case artic of
      "dxf"        -> Nothing
      "susVibHard" -> Just (24 + 0)
      "susLeg"     -> Just (24 + 3)
      "susVibSoft" -> Just (24 + 11)
      "stac"       -> Just (36 + 4)
      "marc"       -> Just (36 + 2)
      "tr1"        -> Just (24 + 5)
      "tr2"        -> Just (24 + 6)
                            
-}

  
