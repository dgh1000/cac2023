

{-# LANGUAGE FlexibleContexts #-}

module Util.Error where
  
import Control.Monad.Except

catchAdd :: MonadError String m => String -> m a -> m a
catchAdd sAdd x =  catchError x (\s -> throwError (sAdd ++ "\n" ++ s))
