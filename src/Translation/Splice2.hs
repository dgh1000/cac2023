module Translation.Splice2 where

import qualified Data.Map as M
import qualified Data.List as L
import Data.Function
import Debug.Trace
import Text.Printf
import Data.Map(Map)
import Data.Maybe
import Control.Monad.State
import Control.Lens
import Score.ScoreData
import Common
import Util.Map
import Util.Exception
import Translation.TimeMap
import Translation


data StaffNameLocMark = StaffNameLocMark String Loc MarkD
 

spliceEverything :: [String] -> ([SNote],[TrRaw]) -> Tr ([SNote],[TrRaw])
spliceEverything splicePts (sns,raws) = "at splice" `trace` (do
  score <- gets $ view score
  atms  <- gets $ view timeMaps
  let locPairs :: [((String,Loc),(String,Loc))]
      locPairs = map (lookupSpliceLocs $ scMarksByStaff score) splicePts
      timePairs :: [(Double,Double)]
      timePairs = L.sortBy (compare `on` fst) $
                  map (computeSpliceTimes atms) locPairs
      deltas = scanl (\x (y,z) -> x+z-y) 0 timePairs
      timePairs2 = zipWith (\(x,y) z -> (x-z,y-z)) timePairs deltas
      out :: ([SNote],[TrRaw])
      out = let x = foldl doSplice (sns,raws) timePairs2 in x
  return out)


lookupSpliceLocs :: Map String (Map Loc [MarkD]) -> String ->
                    ((String,Loc),(String,Loc))
lookupSpliceLocs markMap splicePoint = (beg,end)
  where
    beg = case lMapMaybeCollect (maybeSpliceBeg splicePoint) markMap of
      []  -> throwMine $ printf "splice begin '%s' not found at any loc"
      [x] -> x
    end = case lMapMaybeCollect (maybeSpliceEnd splicePoint) markMap of
      []  -> throwMine $ printf "splice end '%s' not found at any loc"
      [x] -> x


maybeSpliceBeg :: String -> String -> Loc -> MarkD -> Maybe (String,Loc)
maybeSpliceBeg splicePt staffN loc (SpliceBeg p)
  | splicePt `L.isInfixOf` p = Just (staffN,loc)
  | otherwise     = Nothing
maybeSpliceBeg _ _ _ _ = Nothing


maybeSpliceEnd :: String -> String -> Loc -> MarkD -> Maybe (String,Loc)
maybeSpliceEnd splicePt staffN loc (SpliceEnd p)
  | splicePt == p = Just (staffN,loc)
  | otherwise     = Nothing
maybeSpliceEnd _ _ _ _ = Nothing


computeSpliceTimes :: Map String AbsTimeMap ->
                      ((String,Loc),(String,Loc)) -> (Double,Double)
computeSpliceTimes timeMaps (p1,p2) = (go p1,go p2)
  where
    go (st,loc) = lookupTime loc (spLookup st timeMaps)


spLookup k m = case M.lookup k m of {Just x -> x}
    

{-
toSnlm :: Map String (Map Loc [MarkD]) -> [StaffNameLocMark]
toSnlm m = concatMap g1 $ M.toAscList m
  where
    g1 :: (String,Map Loc [MarkD]) -> [StaffNameLocMark]
    g1 (staffN,m2) = concatMap g2 $ M.toAscList m2
      where
        g2 :: (Loc,[MarkD]) -> [StaffNameLocMark]
        g2 (loc,ms) = map (\m -> StaffNameLocMark staffN loc m) ms


getSpliceTimes :: Maybe String -> Tr (Maybe (Double,Double))
getSpliceTimes Nothing = return Nothing
getSpliceTimes (Just spliceName) = do
  timeMaps <- gets tsTimeMaps
  marks <- scMarksByStaff `liftM` gets tsScore
  let marks2 :: [StaffNameLocMark]
      marks2 = toSnlm marks
  -- locate SpliceBegin
  let expand (l,ms) = map (l,) ms
      g2 :: String -> (StaffNameLocMark -> Maybe (String,Loc)) -> (String,Loc)
      g2 errMsg pred = l
        where
          l = case mapMaybe pred marks2 of
            []  -> throwMine $ printf
                   "asked to perform a splice, but there is no %s" errMsg
            [l] -> l
            ls  -> throwMine $ printf ("asked to perform a splice, but "++
                   "there are many %s, at %s") errMsg
                   (concatMap locToString $ map snd ls)
              where
               locToString :: Loc -> String
               locToString l = showLoc2 l ++ " "
      -- locate splice begin point                 
      maybeSpliceBegin :: StaffNameLocMark -> Maybe (String,Loc)
      maybeSpliceBegin (StaffNameLocMark s loc SpliceBeg) = Just (s,loc)
      maybeSpliceBegin _                                  = Nothing
      spliceBeginLoc = g2 "splice begin loc" maybeSpliceBegin
  -- locate point that we are cutting to
  let maybeSplicePoint :: String -> StaffNameLocMark -> Maybe (String,Loc)
      maybeSplicePoint spliceName (StaffNameLocMark s l (SplicePoint s2))
        | spliceName == s2 = Just (s,l)
        | otherwise        = Nothing
      maybeSplicePoint _ _ = Nothing
      splicePointLoc = g2 "splice point to cut to" (maybeSplicePoint spliceName)
      t1 = let timeMap = tmLookup (fst spliceBeginLoc) timeMaps
           in  lookupTime (snd spliceBeginLoc) timeMap
      t2 = let timeMap = tmLookup (fst splicePointLoc) timeMaps
           in  lookupTime (snd splicePointLoc) timeMap
      retVal | t1 <= t2 = Just (t1,t2)
             | otherwise = throwMine $ printf ("error in " ++
                           "toMidi:getSpliceTimes: "++
                           "cut-to time is less than cut-from time")
  return retVal
-}

doSplice (notes,raws) times = let x = (notesOut,rawsOut) in "at doSplice" `trace` x
  where
    notesOut = spliceNotes times notes
    rawsOut  = spliceRaws  times raws


spliceNotes :: (Double,Double) -> [SNote] -> [SNote]
spliceNotes (tCut1,tCut2) snotes = map (closeGap tCut1 tCut2) cutNotes
  where
    cutNotes = mapMaybe (cutSNote tCut1 tCut2) snotes


spliceRaws :: (Double,Double) -> [TrRaw] -> [TrRaw]
spliceRaws (tCut1,tCut2) raws = filter pred raws
  where
    pred r@(TrRaw _ t _ _ _ _) = t < tCut1 || t >= tCut2


cutSNote :: Double -> Double -> SNote -> Maybe SNote
cutSNote tCut1 tCut2 sn
  | needElim  = Nothing
  | needTrunc = Just sn { snOnOff = truncOO origOO tCut1 }
  | needDecap = Just sn { snOnOff = decapOO origOO tCut2 }
  | otherwise = Just sn
  where
    origOO = snOnOff sn
    (et1,et2) = ooHead origOO
    needTrunc = et1 <  tCut1  && et2 >  tCut1
    needDecap = et1 <  tCut2  && et2 >  tCut2
    needElim  = et1 >= tCut1  && et2 <= tCut2

    
ooHead :: [(String,(Double,Double))] -> (Double,Double)
ooHead oo = case oo of
  (_,ts):_ -> ts


truncOO oo tTrunc = ("Splice:trunc",(t1,tTrunc)):oo
  where
    (t1,_) = ooHead oo


decapOO oo tDecap = ("Splice:decap",(tDecap,t2)):oo
  where
    (_,t2) = ooHead oo


closeGap :: Double -> Double -> SNote -> SNote
closeGap tCut1 tCut2 sn
  | et2 <= tCut1 = sn
  | et1 >= tCut2 = sn { snOnOff = newOO }
  where
    (et1,et2) = ooHead $ snOnOff sn
    delta = tCut2 - tCut1
    newOO = ("Splice:close",(et1-delta,et2-delta)) : snOnOff sn
