
module Cac.SimplerC.Test where


-- C1 :: <current steps> <remaining steps> <take modulo by this>
data C1 = C1 [Int] [Int] Int


data S1 = S1 Integer Int

c1_choicesFunc :: C1 -> [S1]
c1
