module Util.EitherError where

import Control.Monad.Except

type StringError = Either String

rethrowAnnotate :: String -> StringError a -> StringError a
rethrowAnnotate _ (Right x) = Right x
rethrowAnnotate s1 (Left s2) = Left (s1 ++ "\n" ++ s2)
