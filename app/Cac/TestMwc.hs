
import Text.Printf
import Control.Monad
import Control.Monad.ST
import Control.Monad.Trans.Class
import Control.Monad.Trans.State.Strict
import Control.Monad.Trans.Except
import System.Random.MWC

----------------------------------------------------------------------
--               Er monad: combines exception, state, and ST. gen will be held in state


data MyExc = MyExc String
           deriving(Show)

data MyState s = MyState (GenST s)

type Er s a = ExceptT MyExc (StateT (MyState s) (ST s)) a

runEr1 :: Er s a -> StateT (MyState s) (ST s) (Either MyExc a)
runEr1 = runExceptT

runEr2 :: Er s a -> GenST s -> ST s (Either MyExc a,MyState s)
runEr2 m g = runStateT (runExceptT m) (MyState g)


testEr1 :: Er s Double
testEr1 = do
  MyState gen <- lift get
  lift $ uniform gen

testEr2 :: Er s [Double]
testEr2 = replicateM 10 testEr1
  

main = do
  v <- withSystemRandom $ asGenST $ \gen -> runEr2 testEr2 gen
  case v of
    (Left exc,_) -> print $ exc
    (Right xs,_) -> forM_ xs (\x -> putStrLn $ printf "%.4f" x)
  
  

----------------------------------------------------------------------
--               Ers monad

data ErsState = ErsState Double

type Ers s = StateT ErsState (ST s)

runErs :: ErsState -> Ers s a -> ST s (a,ErsState)
runErs s m = runStateT m s


type Ers2 s st = ExceptT String (StateT st (ST s))

runErs2 :: Ers2 s st a -> st -> ST s (Either String a,st)
runErs2 m s = runStateT (runExceptT m) s

twos :: GenST s -> Ers s Double
twos gen = do
  ErsState d2 <- get
  d1 <- uniform gen
  return (d1+d2)

t4 :: Er s Double
t4 = do
  MyState gen <- lift get
  uniform gen

t3 :: GenST s -> Ers2 s Double Double
t3 gen = do
  v1 <- lift get
  v2 <- uniform gen
  return $ v1+v2


two :: GenST s -> ST s (Double,Double)
two gen = do
  d1 <- uniform gen
  d2 <- uniform gen
  return (d1,d2) 

main_x = do
  (v1,_) <- withSystemRandom . asGenST $ \gen -> runErs2 (t3 gen) 10
  print v1


