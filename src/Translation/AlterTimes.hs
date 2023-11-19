
module Translation.AlterTimes where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace
import Text.Printf
import Data.Map.Strict(Map)
import Data.Set(Set)
import Translation
import Translation.InstrUtils


-- is this just about arpeggios? wait those don't offer tOff

-- looks at a bunch of SNote
alterTOff :: Bool -> [SNote] -> Tr [SNote]
alterTOff doSepSame notes = do
  -- a function to pair up nominal pitch (not harmonics I think)
  -- and the on time. In preparation for calling M.fromListWith
  -- this appears to map every pitch to all its on times 
  let fn s = (snNomPitch s,[getTOn s])
      begSet = M.map S.fromList $ M.fromListWith (++) $ map fn notes
  return $ map (alterOne doSepSame begSet) notes


type OO = (String,(Double,Double))

-- it looks like alterEnd can be positive for legato and negative for
-- separated.

alterOne :: Bool -> Map Int (Set Double) -> SNote -> SNote
alterOne doSepSame begSet s =
    s { snOnOff = minDurFn . (if doSepSame then gapFunc else id) . endFn $ snOnOff s}
  where
    alterEnd = snAlterEnd s
    sepSame = snSepSame s
    endFn :: [OO] -> [OO]
    endFn oo@((_,(t1,t2)):_)
      | alterEnd > 0 = ("extend",(t1,t2+alterEnd)) : oo
      | alterEnd < 0 = ("trunc" ,(t1,t2+alterEnd)) : oo
      | otherwise = oo
    gapFunc oo@((_,(t1,t2)):_) = case M.lookup (snNomPitch s) begSet of
      Nothing -> oo
      Just s -> case S.lookupGT t1 s of
        Nothing -> oo
        Just x
          | x >= t2+sepSame -> oo
          | otherwise   -> ("sepSame",(t1,x-sepSame)) : oo
    minDur = 0.05
    minDurFn oo@((_,(t1,t2)):_) | t2-t1 >= minDur = oo
                                | otherwise = ("minDur",(t1,t1+minDur)):oo
   
