{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}


import Control.Monad.State
import System.Random

data ReportData t1 t2 = ReportData t1 t2

-- this is rolling my own state monad with a random generator
class Monad m => RandMonad m where
   getGen :: m StdGen
   putGen :: StdGen -> ()

-- this is a class of state monad which logs ReportData:

class Monad m => LogMonad m where
   putReport :: ReportData t1 t2 -> m ()

-- For a particular use case, I declare a type of State monad:

data MyStateData t1 t2 = MyStateData t1 t2
  { theGen     :: StdGen
  , theReports :: [ReportData t1 t2]
  }

type MyState t1 t2 = State (MyStateData t1 t2)

-- And I try to define my instances:

instance RandMonad (MyState t1 t2) where
  getGen   = gets theGen
  putGen g = modify (\s -> s { theGen = g})  

instance LogMonad (MyState t1 t2) where
  putReport r = modify (\s -> s { theReports = r : theReports s})
