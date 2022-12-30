{-# LANGUAGE TypeSynonymInstances, FlexibleInstances #-}

import qualified Data.Map as M
import Data.Map(Map)
import System.Random.MWC
import Control.Monad.Trans
import Control.Monad.Trans.Except
import Control.Monad.Trans.State.Strict
import Control.Monad.ST
import Cac
import Cac.Pcs

data Ers1 s = Ers1
  { e1Gen :: GenST s
  , e1Cache :: Map Pcs (Map Int [Pcs])
  }
  

