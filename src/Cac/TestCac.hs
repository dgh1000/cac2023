
module Cac.TestCac where

import qualified Data.List as L
import Data.Map(Map)
import Text.Printf
import System.Random
import Control.Monad
import Control.Monad.Trans.Except
import Data.Function
import Cac
import Cac.Pcs
import Util.Showable

----------------------------------------------------------------------
--                   evaluation of PC sets


{-

-- thinking out loud:
--
--   have 4-pitch PC set: evaluation will give 4 points for every full statement of PC set, 3
--   points for every independent 3 pitch subset, 2 for every 2


evalFrag :: Pcs -> [Int] -> Double
evalFrag motif frag = 0
  where
    analyzeFrag motif frag


-}


-- create random pcs and analyze it

mkRandFrag :: Int -> Er Pcs
mkRandFrag n = do
  let g :: Er Int
      g = raRandomR (0,11)
  xs <- replicateM n g
  return $ pFromList xs


analyzeRandPcs :: Pcs -> Er (Map Int [Pcs],Pcs)
analyzeRandPcs motif = do
  frag <- mkRandFrag 7
  let subsets = analyzeSubsets motif
  return (analyzeFrag subsets frag,frag)

main_analyzeRandPcs :: IO ()
main_analyzeRandPcs = do
  let motif = pFromList [0,2,3,7]
  g <- newStdGen
  case evalEr g (analyzeRandPcs motif) of
    (Left err,_) -> putStrLn $ showExc err
    (Right (analysis,frag),_) -> do
      let Component _  _ lines = showI analysis
      putStrLn . showiToString $ Component (pShow frag) True lines
  

----------------------------------------------------------------------

testEr_help :: Er Int
testEr_help = return 3

testEr_help2 :: Er Int
testEr_help2 = throwE $ Exc ["Nodin"]

te_1 :: Int -> Int -> Er Int
te_1 x y = withExceptT (addContext report) (te_1_e x y)
  where
    report = printf "x:%d y:%d" x y

te_1_e :: Int -> Int -> Er Int
te_1_e x y | x+y == 3 = throwE $ Exc ["from te_1_e"]
           | otherwise = return $ x+y



testEr :: IO ()
testEr = do
  g <- newStdGen
  case evalEr g (te_1 1 2) of
    (Left e,_)  -> putStrLn $ showExc e
    (Right v,_) -> print v








----------------------------------------------------------------------
--                 test raMaxRandTied

main_raMaxRandTied = do
  g <- newStdGen
  let xs = [(1,'a'),(2,'b'),(2,'c')] :: [(Double,Char)]
      monad :: Er Char
      monad = do
        c <- raMaxRandTied fst xs
        return $ snd c
  case evalEr g monad of
    (Right c,_) -> print c
    
    
  

----------------------------------------------------------------------
--         test oct relation algorithm

main_octRel = do
  let isOctRel (x,y) = x /= y && x `mod` 12 == y `mod` 12
  print $ map isOctRel [(0,0),(0,12),(1,2)]


----------------------------------------------------------------------



  

testPcs = do
  let p = pFromList [3,4]
      x = putStrLn . show . pToList
  mapM_ x $ pAllT p


----------------------------------------------------------------------

main = main_raMaxRandTied
