
module Translation.AlterTimes where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import Debug.Trace
import Text.Printf
import Data.Map.Strict(Map)
import Data.Set(Set)
import Translation
import Translation.InstrUtils


-- is this entirely looking at slurs? or repeated notes
alterTOff :: [SNote] -> Tr [SNote]
alterTOff notes = do
  let fn s = (snNomPitch s,[getTOn s])
      begSet = M.map S.fromList $ M.fromListWith (++) $ map fn notes
  return $ map (alterOne begSet) notes


type OO = (String,(Double,Double))

-- it looks like alterEnd can be positive for legato and negative for
-- separated.

alterOne :: Map Int (Set Double) -> SNote -> SNote
alterOne begSet s =
    s { snOnOff = minDurFn . gapFunc . endFn $ snOnOff s}
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
   
