
module Configs.GOrganConfig where

import qualified Data.Map as M
import Translation
import Common
import Instruments.SynthExamples
import Instruments.GOrgan
import Configs.GenericShapeMasterFunctions


velCurve1 = VelCurve [ (0.45,  1)
                     , (8.55,127) ]

prinz = simpleBracketFunc $
  SimpleBracketFunc "prinz" [] velCurve1 Nothing 


barPlenReed = simpleBracketFunc $
  SimpleBracketFunc "barplenreed" [] velCurve1 Nothing


sym = simpleBracketFunc $
  SimpleBracketFunc "sym" [] velCurve1 Nothing


ped = simpleBracketFunc $
  SimpleBracketFunc "ped" [] velCurve1 Nothing

modPrinz = simpleBracketFunc $
  SimpleBracketFunc "modPrinz" [] velCurve1 Nothing
  

-- ACTUAL BRACKET letters here
bm = M.fromList
  [ ("z", prinz       )
  , ("b", barPlenReed )
  , ("s", sym         )
  , ("p", ped         )
  , ("mz", modPrinz    ) ]


org1 staffNs =
  makeOrgan
    staffNs
      [ ("barplenreed",(0,1))    -- baroque plenum, reed
      , ("prinz"      ,(0,2))    -- prinzipal
      , ("sym"        ,(0,3))    -- symphonic ??
      , ("ped"        ,(0,4))    -- pedal plenum ??
      , ("modPrinz"   ,(0,5)) ]  -- modern prizipal ??
        bm gsFunc1
