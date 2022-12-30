module Instruments.Piano_examples2 where

import qualified Data.Map as M
import Instruments.Piano2
import Instruments.InstrumentsData


{-
example2 stName1 stName2 = Piano2 dests c1 c2
  where
    maxSingleVel = 115
    maxTrillVel  = 100
    dests = M.fromList [ (staffN1, (0,1))
                       , (staffN2, (0,2)) ]
    vCurveSingle = VelCurve [ (0.45, 10)
                            , (8.55, maxSingleVel) ]


    vCurveTrill  = VelCurve [ (0.45, 10)
                            , (8.55, maxTrillVel)
-}


typical_1 staffN1 staffN2 maxSingleVel maxTrillVel =
  let staffData = M.fromList [ (staffN1, (0,1))
                             , (staffN2, (0,2)) ]

      vCurveSingle = VelCurve [ (0.45, 10)
                              , (8.55, maxSingleVel) ]


      vCurveTrill  = VelCurve [ (0.45, 10)
                              , (8.55, maxTrillVel)
                              ]

      piano1 = makePiano "piano1" staffData vCurveSingle
               vCurveTrill
  in piano1

