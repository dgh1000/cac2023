{-# LANGUAGE ExistentialQuantification #-}

import qualified Data.Map as M
import qualified Data.Set as S
import Data.Set(Set)
import Data.Map(Map)


s1 = S.fromList [1 :: Int, 2, 3, 4]
s2 = S.fromList [1 :: Int, 2, 6, 7]

main = putStrLn $ show $ S.difference s2 s1


aIndex :: [a] -> Int -> a
aIndex xs idx = case drop idx xs of
  x:_ -> x


l1 = [1, 2, 3, 4]


x1 = aIndex l1 4


data T = forall a. T
  { tData :: Map String a }


t1 :: T
-- t1 = M.fromList [("foo", 1::Int), ("bar", 2 :: Double)]
t1 = T $ M.empty
-- f1 (T m) = T $ M.insert "foo" (1::Int) m
