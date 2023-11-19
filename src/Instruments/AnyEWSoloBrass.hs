module Instruments.AnyEWSoloBrass where


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

defVelCurve = VelCurve [(0.99,1),(8.01,127)]

ewSoloTrumpet1 staffN destDXF destKS =
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
    qm = anySimpleModExample "in processing ew solo trumpet 1" False
                             (VelCurve [(0.99,1),(8.01,127)]) destDXF 
    df _ artic = case artic of
      "dxf" -> destDXF
      _     -> destKS
    vcf _ artic = defVelCurve
    ksf snote artic = simpleKSChoice snote artic 
      [(Staccato,"stacRR")] "solo trumpet 1" 
      [ ("dxf"       , Nothing)
      , ("expLeg"    , Just $ 24 + 3)
      , ("exp"       , Just $ 24 + 5)
      , ("portLeg"   , Just $ 24 + 2)
      , ("port"      , Just $ 24 + 4)
      , ("susVib"    , Just $ 24 + 6)
      , ("susLeg"    , Just $ 24 + 1)
      , ("sus"       , Just $ 24 + 0)
      , ("stacRR"    , Just $ 24 + 7)
      , ("sfzCrec"   , Just $ 24 + 8)
      , ("slur"      , Just $ 24 + 9)
      ]


ewSoloTrombone staffN destDXF destKS =
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
    qm = anySimpleModExample "in processing ewSoloTrombone" False
                             (VelCurve [(0.99,1),(8.01,127)]) destDXF 
    df _ artic = case artic of
      "dxf" -> destDXF
      _     -> destKS
    vcf _ artic = defVelCurve
    ksf snote artic = simpleKSChoice snote artic 
      [(Staccato,"stacRR")] "solo trombone" 
      [ ("dxf"          , Nothing)
      , ("muteSus"      , Just $ 84 + 1)
      , ("portLeg"      , Just $ 84 + 3)
      , ("portato"      , Just $ 84 + 5)
      , ("qLeg"         , Just $ 84 + 4)
      , ("susLeg"       , Just $ 84 + 2)
      , ("sus"          , Just $ 84 + 0)
      , ("marcF"        , Just $ 84 + 6)
      , ("stacRR"       , Just $ 84 + 7)
      , ("bassSfzCresc" , Just $ 84 + 8)
      ]
