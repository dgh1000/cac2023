module Configs.GenericShapeMasterFunctions where

import Debug.Trace
import Text.Printf
import Instruments
import Common.CommonUtil
import Common
import Util.Exception
import Data.Ratio
import Translation.GenericShape
import Translation

-- master generic shape function
--
--  defintion tempo ratio r: actual tempo is r times base tempo
--
-- R(x,y): means ramp from x to y
--
-- P(x) : means pause from x*beat_length
--
--   u: speed up, then pause before arrival
--         R(1,1+x), P(x)
--       
--   d: slow down, then arrive too soon
--         param: x
--         R(1,1-x), compress from loc2-4*x to loc2 by x/4
--
--   ub: speed up, then pause beforeo arrivation
--
--         R(1, sqrt(1+x)), P(x)
--
--  t1: just for organic-06
--
--  t2: just for at05
gsFunc1 :: GsFunc
gsFunc1 timeSigs c = case gcType c of
    "a"  -> gsFunc1_a timeSigs c
    "b"  -> gsFunc1_b timeSigs c
    "s"  -> gsFunc1_s timeSigs c
    "ss" -> gsFunc1_ss timeSigs c
    "u"  -> gsFunc_cool timeSigs c
    "ub" -> gsFunc_cool timeSigs c
    "d"  -> gsFunc_cool timeSigs c
    "db"  -> gsFunc_cool timeSigs c
    "t1"  -> gsFunc_t1 timeSigs c
    "c"  -> gsOrg02 timeSigs c
    _    -> []




gsFuncErrMsg2 :: String -> GsCombined -> [Utm]
gsFuncErrMsg2 msg (GsCombined staffN loc typ _ _ _) =
  throwMine $ printf ("Error, generic shape on '%s' at %s: for type '%s', "++
                      "%s") staffN (showLoc2 loc) typ msg

gsFunc_t1 timeSigs (GsCombined _ loc typ [] Nothing Nothing) =
    [ UtmRamp Nothing loc locM 1.03 1.03
    , UtmRamp Nothing locM loc2 0.95 0.95]
  where
    locM = case locAddQuar timeSigs loc 2 of {Just d -> d}
    loc2 = case locAddQuar timeSigs loc 3 of {Just d -> d}
    

gsFunc_cool timeSigs (GsCombined _ loc typ [amt] Nothing (Just loc2)) =
    case typ of
    "c" -> [ UtmRamp Nothing loc locM (sqrt $ 1/amt) amt 
            , UtmRamp Nothing locM loc2 amt ((1/amt)*(1/amt)*(1/amt))
            , UtmPause Nothing loc2 (0.1*(amt-1)) ]
    "u" ->  [ UtmRamp Nothing loc loc2 1 (1+amt)
            , UtmPause Nothing loc2 amt ]
    "ub" -> [ UtmRamp Nothing loc loc2 1 (sqrt $ 1+amt)
            , UtmPause Nothing loc2 amt ]
    "d"  -> [ UtmRamp Nothing loc loc2 1 (1-amt)
            , UtmWarp Nothing loc1 loc2 UrsFlat (Left (-amt/4)) ]
    "db" -> [ UtmRamp Nothing loc loc2 1 (sqrt $ 1-amt) 
            , UtmRamp Nothing loc1 loc2 1 2 ]
    where
        loc1 = case locAddQuar timeSigs loc2 (approxRational (-2*amt) 0.001) of
            {Just l -> l}
        diff = locDiffQuar timeSigs loc loc2
        locM = case locAddQuar timeSigs loc (2*diff/3) of {Just d -> d}
        loc1_3 = case locAddQuar timeSigs loc (diff/3) of {Just d -> d}
gsFunc_cool timeSigs (GsCombined st loc typ amts ml rl) =
   throwMine $
     printf
       ("doesn't have proper combination of arguments or locations: type: %s,"
         ++ " begin loc: %s, amts: %s mEnd:(%s)")
           typ (showLoc2 loc) (show amts) (showMaybeLoc rl)

msg1 :: Loc -> Loc -> String
msg1 l l2 = printf "msg1 %s %s" (showLoc2 l) (showLoc2 l2)

showMaybeLoc :: Maybe Loc -> String
showMaybeLoc (Just l) = "Just " ++ showLoc2 l
showMaybeLoc Nothing  = "Nothing"
  
-- 
gsOrg02 timeSigs (GsCombined _ loc typ [amt] Nothing (Just loc2)) =
    case typ of
      "c" -> [ UtmRamp Nothing loc locM (sqrt $ 1/amt) amt
             , UtmRamp Nothing locM loc2 amt ((1/amt)*(1/amt)*(1/amt))
             , UtmPause Nothing loc2 (0.1*(amt-1)) ]
        where
          diff = locDiffQuar timeSigs loc loc2
          locM = case locAddQuar timeSigs loc (2*diff/3) of {Just d -> d}


gsFunc1_a timeSigs (GsCombined _ loc "a" [amtQuars] (Just locL) Nothing)
    = [ UtmWarp Nothing locL loc UrsTowardEnd (Left (-amtQuars))
    , UtmPause Nothing loc amtQuars ]
gsFunc1_a _ c = gsFuncErrMsg2
                "should be: (1) amount, (2) yes left, (3) no right" c

gsFunc1_b timeSigs gs@(GsCombined _ loc "b" [amtQuars] Nothing (Just locR))
    = [ UtmWarp Nothing loc locR UrsFlat (Right amtQuars) ]
gsFunc1_b _ c = gsFuncErrMsg2
                (printf "should be: (1) amount, (2) no left, (3) yes right %s"
                (show c)) c

-- get there too soon
gsFunc1_s timeSigs (GsCombined staffN loc "s" [amt] Nothing
                        (Just loc2))
    = [ UtmRamp (Just staffN) loc loc2 1 (1+amt/2.0)
    , UtmPause (Just staffN) loc2 amt ]

gsFunc1_ss timeSigs (GsCombined staffN loc "ss" [amtQuars] Nothing Nothing)
    = [ UtmWarp (Just staffN) loc1 loc UrsTowardEnd (Left $ -amtQuars)
    , UtmWarp (Just staffN) loc loc2 UrsFromBeg (Left $ amtQuars) ]
    where
    loc1 = case locAddQuar timeSigs loc (-1) of {Just x -> x}
    loc2 = case locAddQuar timeSigs loc (1) of {Just x -> x}

    
    
    {-
    gsFunc1_w timeSigs (GsCombined _ loc "w" (Just amtQuars) Nothing (Just locR))
     = [ UtmWarp Nothing loc locR UrsFlat (Right 
    -}
    
    {-
    gsFunc1_u timeSigs (GsCombined staffN loc "u" (Just t2) Nothing Nothing)
      = [ UtmRamp Nothing loc  loc1 1 t2 
        , UtmRamp Nothing loc1 loc2 t2 1 ]
      where
        loc1 = gsFromJust $ locAddQuar timeSigs loc 3
        loc2 = gsFromJust $ locAddQuar timeSigs loc 6
    -}
    
