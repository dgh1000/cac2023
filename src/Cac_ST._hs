{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

module Cac where

import Data.List(sortBy,maximumBy)
import Data.Function
import System.Random
import System.Random.MWC
import Control.Monad.State
import Control.Monad.ST
import Control.Monad.Primitive
import Control.Monad.Trans.Except
import Control.Monad.Trans.Writer
import Util.Exception
import Util.Showable


--------------------------------------------------------------------------------------
--                     monad Ers for combining MWC random, exceptions, and custom state

data ErsState st s = ErsState
  { ersGen :: GenST s
  , ersState :: st
  }

type Ers st s = ExceptT Exc (StateT (ErsState st s) (ST s))

instance ExcClass (Ers st s) where
  throwExc msg = throwE $ Exc [msg]
  catchExc = catchE
  withExc = withExceptT



runErs :: Ers st s a -> GenST s -> st -> ST s (Either Exc a,ErsState st s)
runErs m gen custom = runStateT (runExceptT m) (ErsState gen custom)


ersRandomR :: Variate a => (a,a) -> Ers st s a
ersRandomR r = do
  gen <- gets ersGen
  uniformR r gen


ersChoose :: [a] -> Ers st s a
ersChoose xs | null xs = throwExc "given null list in ersChoose"
             | otherwise = do
                 idx <- ersRandomR (0::Int, length xs-1)
                 return $ xs !! idx


ersChooseN :: Int -> [a] -> Ers st s [a]
ersChooseN takeN xs
  | null xs = throwExc "given null list in ersChooseN"
  | otherwise = do
      let wrapRand x = do {r <- ersRandomR (0,1::Double); return (r,x) }
      wrapped <- sortBy (compare `on` fst) `liftM` mapM wrapRand xs
      return . map snd $ take takeN wrapped

  
ersMaxRandTied :: (a -> Double) -> [a] -> Ers st s a
ersMaxRandTied eval xs
  | null xs = throwExc "in ersMaxRandTied, null xs"
  | otherwise = do
      let wrapRand x = do {r <- ersRandomR (0,1::Double); return ((eval x,r),x); }
      (snd . maximumBy (compare `on` fst)) `liftM` mapM wrapRand xs








------------------------------------------------------------
--                    class Wr for writer logging


class Monad m => Wr m where
  tellSi :: ShowItem -> m ()
  
--------------------------------------------------------------------------------
{-
            the Mwc class didn't work out too well: couldn't get instances to typecheck




class PrimMonad a => Mwc a where
  getGenMwc :: a (GenST s)


mwcRandomR :: (Variate a, Mwc m) => (a,a) -> m a
mwcRandomR r = do
  gen <- getGenMwc
  uniformR r gen


mwcChoose :: (Variate a, Mwc m, ExcClass m) => [a] -> m a
mwcChoose xs | null xs = throwExc "given null list in raChoose"
             | otherwise = do
                 idx <- mwcRandomR (0::Int, length xs-1)
                 return $ xs !! idx


mwcChooseN :: (Variate a, Mwc m, ExcClass m) => Int -> [a] -> m [a]
mwcChooseN takeN xs
  | null xs = throwExc "given null list in raChooseN"
  | otherwise = do
      let wrapRand x = do {r <- mwcRandomR (0,1::Double); return (r,x) }
      wrapped <- sortBy (compare `on` fst) `liftM` mapM wrapRand xs
      return . map snd $ take takeN wrapped

  
mwcMaxRandTied :: (Variate a, Mwc m, ExcClass m) => (a -> Double) -> [a] -> m a
mwcMaxRandTied eval xs
  | null xs = throwExc "in raMaxRandTied, null xs"
  | otherwise = do
      let wrapRand x = do {r <- mwcRandomR (0,1::Double); return ((eval x,r),x); }
      (snd . maximumBy (compare `on` fst)) `liftM` mapM wrapRand xs


-}


------------------------------------------------------------
--                   class Ra for random numbers

class Monad a => Ra a where
  getGen :: a StdGen
  putGen :: StdGen -> a ()


  

raRandomR :: (Random a, Ra m) => (a,a) -> m a
raRandomR x = do
  g <- getGen
  let (r,g') = randomR x g
  putGen g'
  return r


raShuffle :: (Random a, Ra m) => [a] -> m [a]
raShuffle xs = do
  let p x = do
        r <- raRandomR (0,1)
        return (r::Double,x)
  pairs <- sortBy (compare `on` fst) `liftM` mapM p xs
  return $ map snd pairs



raChoose :: (ExcClass m, Ra m) => [a] -> m a
raChoose xs | null xs = throwExc "given null list in raChoose"
            | otherwise = do
                 g  <- getGen
                 let (index,g') = randomR (0::Int, length xs-1) g
                 putGen g'
                 return $ xs !! index

raChooseN :: (ExcClass m, Ra m) => Int -> [a] -> m [a]
raChooseN takeN xs
  | null xs = throwExc "given null list in raChooseN"
  | otherwise = do
      let wrapRand x = do {r <- raRandomR (0,1::Double); return (r,x) }
      wrapped <- sortBy (compare `on` fst) `liftM` mapM wrapRand xs
      return . map snd $ take takeN wrapped

  
raMaxRandTied :: (ExcClass m, Ra m) => (a -> Double) -> [a] -> m a
raMaxRandTied eval xs
  | null xs = throwExc "in raMaxRandTied, null xs"
  | otherwise = do
      let wrapRand x = do {r <- raRandomR (0,1::Double); return ((eval x,r),x); }
      (snd . maximumBy (compare `on` fst)) `liftM` mapM wrapRand xs
      


             


------------------------------------------------------------------------------
--       class ExcClass for ability to throw exceptions of type Exc


class Monad m => ExcClass m where
  throwExc :: String -> m a
  catchExc :: m a -> (Exc -> m a) -> m a
  withExc :: (Exc -> Exc) -> m a -> m a

data Exc = Exc [String]
           deriving(Show)


addContext :: String -> Exc -> Exc
addContext s (Exc ss) = Exc $ s:ss


showExc :: Exc -> String
showExc (Exc ss) = concatMap (\x -> x ++ "\n") ss


-------------------------------------------------------------------
--             type Er: combines random numbers and throwing Exc exceptions

type Er = ExceptT Exc (WriterT [ShowItem] (State StdGen))

-- this way we get the random seed back even if 
-- runEr :: StdGen -> Er a -> (Either Exc a,StdGen)
-- runEr s = flip runState s . runWriterT . runExceptT

runEr2 :: Er a -> (WriterT [ShowItem] (State StdGen) (Either Exc a))
runEr2 m = runExceptT m

runEr3 :: Er a -> (State StdGen (Either Exc a,[ShowItem]))
runEr3 m = runWriterT $ runExceptT m                  


runEr :: StdGen -> Er a -> ((Either Exc a,[ShowItem]),StdGen)
runEr g m = runState (runWriterT $ runExceptT m) g

evalEr :: StdGen -> Er a -> (Either Exc a,[ShowItem])
evalEr g = fst . runEr g

-------- now must make Er an instance of Ra and ExcClass

instance Ra Er where
  getGen = get
  putGen x = put x


instance ExcClass Er where
  throwExc s = throwE $ Exc [s]
  catchExc = catchE
  withExc = withExceptT



instance Wr Er where
  tellSi s = lift (tell [s])

----------------------------------------------------------------------


type Rio = StateT StdGen IO

rioChoose :: [a] -> Rio a
rioChoose xs | null xs = error "given null list in rioChooseItem"
             | otherwise = do
                 gen  <- get
                 let (index,gen') = randomR (0::Int, length xs-1) gen
                 put gen'
                 return $ xs !! index


rioRandomR :: Random r => (r,r) -> Rio r
rioRandomR (lo,hi) = do
  g <- get
  let (value,g') = randomR (lo,hi) g
  put g'
  return value


rioShuffle :: [a] -> Rio [a]
rioShuffle xs = do
  let p x = do
        r <- rioRandomR (0,1) :: Rio Double
        return (r,x)
  pairs <- sortBy (compare `on` fst) `liftM` mapM p xs
  return $ map snd pairs

----------------------------------------------------------------------

data ShapeFunc = SfTable
  { sfBeg    :: Double
  , sfEnd    :: Double
  , sfTables :: [(Double,Double)]
  }


sfLookup :: Double -> ShapeFunc -> Er Double
sfLookup x (SfTable beg end points)
  | x < beg = throwExc "sfLookup"
  | x > end = throwExc "sfLookup"

