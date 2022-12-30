module Instruments.Ir where

import qualified Data.Map.Strict as M
import Data.Map.Strict(Map)
import Debug.Trace
import Text.Printf
import Control.Arrow
import Control.Lens
import Control.Lens.TH
import Instruments
import System.Environment
import Translation
import Translation.InstrUtils
import Translation.RunOnce
import Translation.GenericShape
import Configs.GenericShapeMasterFunctions
import Common
import Instruments.Any
import Util.Exception


defVelCurve = VelCurve [(0.99,40),(8.01,127)]


-- dest1 is KS, dest2 is artificial harmonics
irViolin :: String -> (Int,Int) -> (Int,Int) -> AnySimple
irViolin staffN dest1 dest2 = AnySimple
    { _anySimpleAccentAmt = 0.75
    , _anySimpleStacFn = return
    , _anySimpleEndAlterFn = updateEndAlter True
    , _anySimpleArpFn = return
    , _anySimpleModFn = qm
    , _anySimpleDestFn = df
    , _anySimpleVelSource = VsCurve (const defVelCurve)
    , _anySimpleKsFn = ksf
    , _anySimpleVolFn = const (Left 127)
    , _anySimpleExprFn = const (Left 127)
    , _anySimpleAliases = [("ord", "ord")]
    , _anySimpleStaffN = staffN
    }
    where
        qm = anySimpleModExample "in processing irViolin" True
                             (VelCurve [(0.99,15),(8.01,127)]) dest1
        df snote artic = case artic of
            "harm"  -> dest2
            "harmG" -> dest2
            "harmD" -> dest2
            "harmA" -> dest2
            "harmE" -> dest2
            _       -> dest1 
        ksf sn artic = case artic of
            "ord"       -> ksf_ord sn
            "ordNonVib" -> Just 25
            "trem"      -> Just 26
            "fp"        -> Just 27
            "sfz"       -> Just 28
            "stac"      -> Just 29
            "pizz"      -> Just 30
            "pizzSecco" -> Just 31
            "tr2"       -> Just 32
            "tr1"       -> Just 33
            "05sec"     -> Just 34
            "10sec"     -> Just 35
            "05"        -> Just 34
            "10"        -> Just 35
            "harm"      -> Just 24 -- deliberately has no effect
            "harmG"     -> Just 36
            "harmD"     -> Just 37
            "harmA"     -> Just 38
            "harmE"     -> Just 39
        ksf_ord sn
           | diff < 0.25 = Just 34
           | diff < 0.5 = Just 35
           | otherwise  = Just 24
           where 
             diff = case snOnOff sn of
                ((_,(tOn,tOff)):_) -> tOff - tOn
                
             tOn  = fst . snd . head . snOnOff $ sn 
             tOff = snd . snd . head . snOnOff $ sn 

-- makeAnySimple :: String -> GsFunc -> AnySimple -> MetaInstr

mkIrViolin :: String -> (Int,Int) -> (Int,Int) -> MetaInstr
mkIrViolin staffN destKS destHarm = 
    makeAnySimple staffN gsFunc1 (irViolin staffN destKS destHarm)
