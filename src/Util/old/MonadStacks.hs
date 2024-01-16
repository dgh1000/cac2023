module Util.MonadStacks(ErrorRand,runEW, runER, evalER
                       , ErrWri) where

import Control.Monad.State
import Control.Monad.Except
import Control.Monad.Writer
import Util.UtilData

runEW m = runWriter (runExceptT m)

runER m s = runState (runWriterT (runExceptT m)) s
evalER m s = fst $ fst $ runER m s

{-
runER_IO :: ErrorRand a -> (a -> IO()) -> RandData -> IO ()
runER_IO m actFn rd = do
  case evalER m rd of
    Left err -> putStrLn err
    Right value -> actFn value
-}

