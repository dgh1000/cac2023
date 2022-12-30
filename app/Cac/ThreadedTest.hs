
import Text.Printf
import System.Random
import System.CPUTime
import Control.Monad.Trans.State
import Control.Monad.Trans.Except
import Control.Monad
import Control.Concurrent
import Control.Parallel (par, pseq)
import Control.DeepSeq
import System.Directory
import Cac

data EriState = EriState
  { esGen :: StdGen
  , esCustom :: Int
  }

type Eri = ExceptT Exc (StateT EriState IO)

runEri1 :: Eri a -> StateT EriState IO (Either Exc a)
runEri1 m = runExceptT m

runEri2 :: EriState -> Eri a -> IO (Either Exc a,EriState)
runEri2 g m = runStateT (runEri1 m) g


doALot :: Int -> Double -> IO Double
doALot x y = return $ sum $ map (const $ sin 1.5) [1..fromIntegral x]

doALot2 :: Int -> StdGen -> IO Double
doALot2 n gIn = do
  let f :: (Double,StdGen) -> (Double,StdGen)
      f (x,g) = randomR (0,1) g
      s = sum . map fst . take n $ iterate f (0,gIn)
  return s


doALot2_mvar :: MVar Double -> Int -> StdGen -> IO ()
doALot2_mvar m n g = do
  result <- doALot2 n g
  putMVar m result
  

test1 :: IO ()
test1 = do
  forkIO (writeFile "nodin.txt" "some stuff\n")
  doesFileExist "nodin.txt" >>= print


-- test2: MVar
test2 = do
  m <- newEmptyMVar
  forkIO $ do
    v <- takeMVar m
    putStrLn $ "received " ++ show v
  putStrLn "sending"
  threadDelay 3000000
  putMVar m "wake up!"

-- test3: several random computations
test3 = do
  g1 <- newStdGen
  m1 <- newEmptyMVar
  forkIO $ doALot2_mvar m1 8000000 g1
  g2 <- newStdGen
  m2 <- newEmptyMVar
  forkIO $ doALot2_mvar m2 8000000 g2
  putStrLn "waiting ...."
  result1 <- takeMVar m1
  putStrLn $ show result1
  result2 <- takeMVar m2
  putStrLn $ show result2  

test4_sort :: (Ord a) => [a] -> [a]
test4_sort (x:xs) = lesser ++ x:greater
  where
    lesser = test4_sort [y | y <- xs, y < x]
    greater = test4_sort [y | y <- xs, y >= x]
test4_sort _ = []
                      

parSort :: (NFData a, Ord a) => [a] -> [a]
parSort (x:xs) = force greater `par` (force lesser `pseq`
                                            (lesser ++ x:greater))
  where lesser = parSort [y | y <- xs, y < x ]
        greater = parSort [y | y <- xs, y >= x]
parSort _ = []

main_rand = do
  gen <- newStdGen
  t1 <- getCPUTime
  x <- doALot2 2000000 gen
  print x
  t2 <- getCPUTime
  let td = (fromIntegral $ t2-t1)/1000000000000 :: Double
  putStrLn $ printf "CPU time: %f" td


main = test3


  

