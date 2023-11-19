module Instruments.AnyMoog where

import qualified Data.Map as M
import Instruments.Any
import Instruments
import Translation.Curves
import Common
import Translation.GenericShape
import Configs.GenericShapeMasterFunctions
import Translation.InstrUtils (updateStac, updateEndAlter)

-- Available MIDI control numbers
--        3   Undefined
--        9   Undefined
--  12 - 31   Undefined, "general purpose"
--  32 - 63   Listed as 0-31 LSB. Hmm.
--  70 - 79   Generic Sound Controller
--  80 - 83   Generic, "on/off switch"
--  85 - 87   Undefined
--  89 - 90   Undefined
--  91 - 95   "Effect Depth" such as tremolo and phaser. 
-- 102 -119   Undefined
-- 
-- 1 + 1 + 20 + 10 + 4 + 3 + 2 + 5 + 18

-- the easiest thing to do is include all control messages
-- with every note, but then remove them. They would probably
-- appear as a ModifCtrl constructor of Modif type.
-- 

-- Moog questions SEPT 2023: 
--   Keyboard controller signals
--   - aftertouch
--   - mod
--   - velocity
--
-- It might be nice to control these independently, but I have
-- only one source of a control curve: the curves that cac
-- generates off the dynamic marks in the score. I would have
-- to add some kind of control mark 
--
-- So given that we have only one available curve, it seems
-- the options are
--   - choose whether to apply that curve to velcity
--   - choose whether to apply that curve to mod
--
-- But it's simpler than that. If we don't want to use
-- velocity in our patch, we simply don't hook up the VEL
-- signal. Likewise, the MOD signal.
--
-- So it seems best to send velocity and mod signals pinned
-- to the dynamic curve at all times.

-- okay why don't we try it sending config before every note and
-- see what happens 

-- loading 

makeMoog staffN dest allLegato =
  makeAnySimple staffN gsFunc1 True
  AnySimple { _anySimpleAccentAmt  = 0.6
            , _anySimpleStacFn     = updateStac
            , _anySimpleEndAlterFn = updateEndAlter allLegato
            , _anySimpleArpFn      = return
            , _anySimpleModFn      = moogMod
            , _anySimpleDestFn     = \_ -> const dest
            , _anySimpleVelSource  = VsCurve (\_ _ -> VelCurve [(0.99,1),(8.01,127)])
            , _anySimpleKsFn       = \_ -> const Nothing
            , _anySimpleModifFn    = \_ _ -> []
            , _anySimpleVolFn      = const (Left 127)
            , _anySimpleExprFn     = const (Left 127)
            , _anySimpleAliases    = []
            , _anySimpleStaffN     = staffN
            , _anySimpleConfigs    = M.empty
            }
  where
    moogMod = anySimpleModExample "in processing moog" False
              (VelCurve [(0.99,1),(8.01,127)]) dest 
