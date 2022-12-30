-- master motive
--
--   idea is that entire piece is built from some master motive that is varied
--   in one of several ways. The master motive is essentially a small
--   composition, in which pitches and rhythms are literal, and notes can be
--   categorized in different ways, perhaps suggesting what notes can be
--   repeated and which not. which are structural and which not.
--

module Cac.MmData where

import qualified Data.Set as S
import Data.Set(Set)

data Note = Note
  { nPitch      :: Int
  , nTimes      :: Times
  , nDyn        :: Double
  , nCategories :: Set String
  , nTimbre     :: String
  }


