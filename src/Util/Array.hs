module Util.Array where

import Data.Array
import Data.Ix
  
safeIdx :: Ix a => String -> a -> Array a b -> b
safeIdx msg ix arr | ix >= lo && ix <= hi = arr ! ix
                   | otherwise = error msg
  where
    (lo,hi) = bounds arr
    
