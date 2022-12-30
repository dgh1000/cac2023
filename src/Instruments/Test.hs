{-# LANGUAGE ExistentialQuantification #-}

module Instruments.Test where

import qualified Data.Map.Strict as M
import Data.Map.Strict(Map)
import Instruments.InstrumentsData 

data Common1 = Common1
  { iName :: String
  , iStaffNs :: [String]
  }

data Instr1 = forall s. Instr1
  { iCommon   :: Common1 
  , iFn       :: Common1 -> s -> String
  , iSpecific :: s
  }

instrList :: [Instr1]
instrList = [ Instr1 (Common1 "foo" []) fn1 1.0
            , Instr1 (Common1 "bob" []) fn2 'a' ]
 
fn1 :: Common1 -> Double -> String
fn1 _ x = show $ 2*x

fn2 :: Common1 -> Char -> String
fn2 _ x = show x
