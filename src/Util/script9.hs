{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import qualified Data.Map as M
import Data.Map(Map)






x1_0 :: Map Double [Int]
x1_0 = M.fromList [ (3.0, [1,2,3])
                  , (6.0, [4,5,6])
                  ]

x1_1 = M.fromList [ (11.0,[11]) ]

x1 :: Map String (Map Double [Int])
x1 = M.fromList [ ("s0", x1_0)
                , ("s1", x1_1)
                ]
