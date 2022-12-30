{-# LANGUAGE TemplateHaskell, MultiParamTypeClasses,
    FunctionalDependencies #-}

import Control.Lens
import Control.Lens.TH

data D = D
  { _dFoo :: Int
  , _dBar :: Char
  }

makeFields ''D

main = do
  let d1 = D 3 'c'
  print $ d1 ^. foo
  
