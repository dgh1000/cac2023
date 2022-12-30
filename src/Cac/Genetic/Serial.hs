module Cac.Genetic.Serial where

import Data.Serialize
import qualified Data.ByteString as B

import Cac.Genetic.Data

instance Serialize Comp where
  put (Comp ns) = do put (1 :: Int)
                     put ns
  get = do v <- get :: Get Int
           case v of
             1 -> Comp <$> get


instance Serialize Note where
  put n = do put (1 :: Int)
             put $ nId n
             put $ nTBeg n
             put $ nTEnd n
             put $ nTOffset n
             put $ nPit n
             put $ nDyn n
             put $ nSounding n
  get = do version <- get :: Get Int
           case version of
             1 -> Note <$> get <*> get <*> get <*> get <*> get <*> get <*> get


writeComp :: String -> Comp -> IO ()
writeComp fileName comp = B.writeFile fileName $ encode comp


readComp :: String -> IO Comp
readComp fileName = do
  c <- decode <$> B.readFile fileName
  case c of
    Left s  -> error s
    Right d -> return d
