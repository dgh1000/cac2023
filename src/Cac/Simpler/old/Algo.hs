module Cac.Simpler.Algo where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import qualified Util.Array as UA
import Data.Array 
import Data.Set(Set)
import Data.Maybe
import Cac.Simpler.SimplerData
import Util.Math


data Context = Context
  { cnPitchMotives   :: PitchMotives
  , cnPitchHist      :: PitchHistory
  , cnRhythmMotives  :: RhythmMotives
  , cnRhythmHist     :: RhythmHistory
  }
  

cnAlterPitchHist :: (PitchHistory -> PitchHistory) -> Context -> Context
cnAlterPitchHist = error "cnAlterPitchHist"


----------------------------------------------------------------------


nextPitchOpts :: Comp -> Context -> [(PitchOpt,Context)] 
nextPitchOpts comp ctx
  | ptr >= len = nextPitchOpts' comp ctx
  | otherwise  = [(newOpt, cnAlterPitchHist (phIncrPtr newOpt) ctx)]
  where
    -- mots = psMotives $ cnPitchMotives ctx
    ph@(PitchHistory form motId ptr len optHistIn _) = cnPitchHist ctx
    newOpt = phLookupOpt motId (ptr+1) ph


nextPitchOpts' :: Comp -> Context -> [(PitchOpt,Context)]
nextPitchOpts' comp ctx = map go $ pmIndices pitchMotives
  where
    pitchMotives = cnPitchMotives ctx
    go motId = error "foo"
      where
        motive = pmLookup motId pitchMotives
        len = pmMotiveLength motId
        opt = UA.safeIdx ",;msn29" 0 motive
        ctxOut = ctx {cnPitchHist = phNew modId motiveLenng}
        

 
pitchOptToElems :: Comp -> PitchOpt -> [Int]
pitchOptToElems comp opt
  | isNothing mTess && isNothing mLeap = error "kn23..0"
  | otherwise                          = S.toList . f1 $ f2 fullSet
  where
    PitchOpt mTess mLeap = opt
    fullSet = S.fromList [cAbsMinPit comp..cAbsMaxPit comp]
    PitchHistory form motId ptr _ _ elemsHist = cPitchHist comp
    lastPitch = case elemsHist of
      PitchElem p:_ -> p
    f1 pitchSet = case mTess of
      Nothing -> pitchSet
      Just t  -> let (t1,t2) = tessRange comp t
                 in  S.intersection pitchSet $ S.fromList [t1..t2]
    f2 pitchSet = case mLeap of
      Nothing      -> pitchSet
      Just (lo,hi) -> S.intersection pitchSet $ S.fromList
                      [lastPitch+lo..lastPitch+hi]

{-
nextTime :: Comp -> RhythmModel -> ElemChoice RhythmElem ->
            (Int,Maybe (ElemChoice RhythmElem))
nextTime comp model choice = (tLast+delta,nextRhythmElem rhythmModel choice)
  where
    ElemChoice (RhythmElem r) name ptr _ form = choice
    tLast0 = case M.maxViewWithKey $ cNotes comp of
      Just ((t,_),_) -> t
    tLast = fromIntegral $ tLast0*1000
    delta = round $ 1000*fromRational r*cBeatDur comp
-}
        
{-

nextRhyMotiveElem :: RhythmModel -> ElemChoice RhythmElem ->
                  Maybe (ElemChoice RhythmElem)
nextRhyMotiveElem model (ElemChoice _ name p len form)
  | p >= len    = Nothing
  | otherwise   = Just $ ElemChoice (error "foo") name (p+1) len form
  where
    es = aLookup name (rmAll model)
    e = case form of
      MfP -> es `aIndex` p
      MfR -> es `aIndex` (len-1-p)
-}
      
aIndex :: [a] -> Int -> a
aIndex xs idx = case drop idx xs of
  x:_ -> x


aArrLk :: Array Int a -> Int -> a 
aArrLk arr idx | lo <= idx && idx <= hi = arr ! idx
  where
    (lo,hi) = bounds arr
  
    
aLookup k m = case M.lookup k m of {Just x -> x}


arrLkMin :: Array Int a -> a
arrLkMin arr = arr ! (fst $ bounds arr)
 

tessRange :: Comp -> Double -> (Int,Int)
tessRange comp pos = (round sLo,round sHi)
  where
    (pitLo,pitHi) = cTess comp
    tol = cTessTolerance comp
    sHi = scaleClip 0 (pos+tol) 1 pitLo pitHi
    sLo = scaleClip 0 (pos-tol) 1 pitLo pitHi
    
