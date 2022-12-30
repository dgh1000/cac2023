{-# LANGUAGE TupleSections #-}

module Score.ParseMarks where


import qualified Data.Map as M
import qualified Data.List as L
import Debug.Trace
import Data.Maybe
import Text.Printf
import Control.Monad
import Data.Map(Map)
import Text.Parsec
import Text.Parsec.String
import Score.ScoreData
import Util.Parse
import Util.Math
import Util.Exception
import Util.Map
import Common
import Common.CommonUtil



{-
          OLD OLD OLD
    OLD STUFF CAN BE FOUND IN old/ParseMarksDocs.hs


-}


{-

LEVEL 1 tempos

  T=100  (Q is no longer necessary)

    set tempo to 100 quarters per minute

  T=a

    set tempo to number of beats in variable a

  T=9%10a

    set tempo to 9/10 of a

  =T=100

    
  either "rit." or "accel."

    either of these specifies a ramp from prevailing tempo to next marked
    tempo. it treats "rit." and "accel." the same; the next marked tempo
    determines whether the tempo will speed up or slow down


variables to indicate a number of beats

  a=90

    set a to 90 quarters

  a=3%2b

    set variable a to 3/2 variable b

  a=3%2

    set a to 3/2 quarters

level 2 tempos, or 'tempo ramps' (used to be called 'ramps')

  Further change: now single = will be accepted as ramp indicator character

    (Legacy form supported for now.)

  In the following we call the prevailing tempo P

  3:2=    =1:2

    ramp from 3/2 P to 1/2 P. return to P afterward.

  1=   =2=    =4:5

    ramp from P to 2 P, then back to 4/5 P, and return to P

  1=   =1:2|4:5=  =1

    ramp from P to 1/2 P, then sudden change to 4:5 P, ramping back up to P

level 1 dynamics

  basic form (some of these will be in XML as direction words, and some as
  direction dynamics

     pppp, ppp, pp, etc. occurring below the staff

  ramp form

     pp**    p|f*

level 2 dynamics: always ramps, and bracketed by =

    +:2=   =-:2

    +:2=     =+1

    +:2=   =-:3=    =+:2

    can put a start and an end at same location

pause

  ps@5

    new form: pause for 50 milliseconds

  ps%8

    pause for 1/8 of a quarter

  ps2%7

    pause for 2/7 of a quarter

  ps1.5

    pause for 1.5 quarters

arpeggio and staccato durations

  arp@5, stac@7

    updated form of specifying 10's of milliseconds


rit/accel



splice

  //

    marks beginning of splice

  /a

    mark possble end of splice, each one marked by a letter such as $b, $c
    etc.


trill/tremolo

  there is a trill scale expressing minimum to maximum trill rate, in which 0
  is minimum (perhaps 4 notes/sec) and 9 is maximum (perhaps 15 notes/sec)

  t^25-93v

    next trill will start at rate of 2, continue for 5 units accelerating
    toward rate of 9, then continue for 3 units. Total duration is 8 units. A
    unit in absolute terms will be 1/8 of the duration of the trill. Trill
    will start on upper note and end on lower note.

  t^25-93^

    same as above but trill will start AND end on upper note 

  tv25-95-00^

    trill will start on lower note. rate 2, ramp for duration 5 units to 9,
    ramp for 5 units to 0, stay at 0 for 0 units. duration of unit is 1/10
    total length of trill. trill will end on upper note.

  trates:4,15

    set trill rate scale. The first number, 4, will be the rate of any trill
    marked as rate 0. The last number, 15 will be rate of a trill marked as
    rate 9. In-between rates are scaled linearally.


patterns (tempo and dynamics patterns)

  pat:<name>

    apply rhythmic and dynamics patterns from pattern statement in the config
    with name <name> between this mark and the next pat: mark or the end of
    the document


bracketed areas

  [abc   abc]    : bracketed area of name "abc"
  [ab,:2  ab]     : parameter passed to bracket
  [ab   |ab   ab]  : center point of bracket "ab"


  do we want to use brackets to express shapes as well? would we like to
  specify a center point?

instrument technique

  some words are recognized as being instrument technique:

    arco, pizz., DXF

patch: a general term for regions of sound within a staff

  different regions may be translated into MIDI differently

  $p:name

Generic shape

   hmm, haven't completed this.

   (a  (a,0.2  (a,0.2,0.1   a)  !a

Control Shape

   <a   <a,0.2   <a,0.2,0.3    a>
  

Level one dynamic

   left: 7*        right: *3

level two dynamic

  left: +0.2*  right: *-0.3

  left/right: (not sure how I intended this to be interpreted) "+0.1

&name

  control setting: a set of midi controller numbers and associated values to
  be sent just before the loc at which this mark is located




  



-}

-- NEW BOUNDARY FORMS
--
-- boundary
--
--   |   or +:8|   (minus form not allowed)
--
--
-- adjusts
--
--   +:2   -a  -:2a
--
-- staff adjusts
--
--   ^+:2  etc.
--

----------------------------------------------------------------------
-- Generic Shapes
--
--
--   Three types of shapes.
--
--      center only, without param: !a
--
--      center only, with param: !a1,u1     !a1,:3
--
--   left/right parentheses
--
--     without param:     (a1    a1)
--
--     with param:  (a1,u1   a1)
--
--
--     with center point, without param
--
--                  (a1      !!a1        a1)
--
--     with center point, with param
--
--                  (a1,:3     !!a1        a1)
--
--
--   New FEBRUARY 2019
--
--     Can have multiple pararmeters. Separated by commas.
--
--   OLD OLD OLD OLD OLD
--
--   The shape center is ! plus an alphabetic character.
--
--     !a
--
--   If the shape has a right end, that will be )a. Left end will be (a
--
--   Some possibilities
--
--    (a !a
--    (a !a )a
--    !a )a
--    !a
--
-- We need to write generic functions that change these to UnitTimeMod
-- 
--
computeWordMarks :: Map String (Map Loc [WordDirection]) ->
                    Map Loc (Map String [MarkD])
computeWordMarks m = computeWordMark_x ma mb
  where
    ma = M.map (lMapMaybe maybeAbove) m
    mb = M.map (lMapMaybe maybeBelow) m
    maybeAbove (WdAbove s) = Just s
    maybeAbove _           = Nothing
    maybeBelow (WdBelow s) = Just s
    maybeBelow _           = Nothing


computeWordMark_x :: Map String (Map Loc [String]) ->
                     Map String (Map Loc [String]) ->
                     Map Loc (Map String [MarkD])
computeWordMark_x mAbove mBelow = M.unionWith (M.unionWith (++))
    marksAbove marksBelow 
  where
    -- marksBleow
    marksBelow' :: Map Loc (Map String [MarkN])
    marksBelow' = flipMap $ M.map parseWordBelow mBelow
    marksBelow :: Map Loc (Map String [MarkD])
    marksBelow = M.mapWithKey toMM marksBelow'
    toMM :: Loc -> Map String [MarkN] -> Map String [MarkD]
    toMM loc m = M.map (map (toMarkD vars loc)) m
    z :: Map String (Map Loc [RawMark NumVar])
    z = M.map parseWord mAbove
    x :: Map Loc (Map String [RawMark NumVar])
    x = flipMap z
    vars = computeVarMap x
    marksAbove :: Map Loc (Map String [MarkD])
    marksAbove = M.mapWithKey (rawMarkToMarkD vars) x
    -- merge Map Loc (Map String [MarkD]), Map Loc (Map String [MarkD])
    -- union in which the merging function is a union in which merging is ++
    

parseWordBelow :: Map Loc [String] -> Map Loc [MarkN] 
parseWordBelow = M.mapWithKey (\k v -> map (f k) v)
  where
    f loc s = case parse dynamic "" s of
      Left err -> throwMine $ printf "BELOW word mark parse error at %s: %s:%s"
                  (showLoc2 loc) (show err) s
      Right w -> w


parseWord :: Map Loc [String] -> Map Loc [RawMark NumVar]
parseWord = M.mapWithKey (\k v -> map (f k) v)
  where
    f loc s = case parse rawMark "" s of
      Left err -> throwMine $ printf "word mark parse error at %s: %s"
                  (showLoc2 loc) (show err)
      Right w  -> w


rawMarkToMarkD :: Variables -> Loc -> Map String [RawMark NumVar] ->
           Map String [Mark Double]
rawMarkToMarkD vars loc = lMapMaybe h
  where
    g :: NumVar -> Double
    g = computeNumVar vars loc
    h :: RawMark NumVar -> Maybe (Mark Double)
    h (RmMark m)   = Just $ toMarkD vars loc m
    h _            = Nothing


toMarkD :: Variables -> Loc -> MarkN -> MarkD
toMarkD vars loc mIn = f mIn
  where
    g = computeNumVar vars loc
    f :: Mark NumVar -> Mark Double
    f (SymbolMark s i) = SymbolMark s i
    f (SetTempo nv1 nv2 flag) = SetTempo (fmap g nv1) (g nv2) flag  
    f (ArpDelta d) = ArpDelta d
    f (StacDur d) = StacDur d
    f (Trunc d) = Trunc d
    f (LTrunc d) =  LTrunc d
    f (Extend d) =  Extend d
    f (AbsWarp s nv1) =  AbsWarp s $ g nv1
    f (Pause nv1) =  Pause $ g nv1
    f (PostPause nv1) =  PostPause $ g nv1 
    f W =  W
    f (RampBeg nv) =  RampBeg $ g nv
    f (RampEndBeg nv1 nv2) =  RampEndBeg (g nv1) (g nv2)
    f (RampEnd nv1) =  RampEnd $ g nv1
    f RitAccel = RitAccel
    f (TrillShapeMark ts) =  TrillShapeMark ts
    f (TremShapeMark ts) =  TremShapeMark ts
    f (Artic s) =  Artic s
    f (Patch s) =  Patch s
    f (BracketL s s2 v) =  BracketL s s2 (fmap g v)
    f (BracketR s)           = BracketR s
    f (Boundary2 nv)         = Boundary2 $ fmap g nv
    f (Adjust2 fl d nv1)     = Adjust2 fl d (g nv1)
    f (MidiCtrl cn nv1 _)    = MidiCtrl cn (g nv1) (round $ g nv1)
    f (GenericShape c)       = GenericShape (k c)
    f (Lev1DynL v)           = Lev1DynL v
    f (Lev1DynR v)           = Lev1DynR v
    f (Lev1DynLR v1 v2)      = Lev1DynLR v1 v2
    f (Lev2DynL sign nv)     = Lev2DynL sign (g nv)
    f (Lev2DynR sign nv)     = Lev2DynR sign (g nv)
    f (Lev2DynLR sign nv)    = Lev2DynLR sign (g nv)
    f (CtrlSetting c)        = CtrlSetting  c
    f (SpliceBeg s)     = SpliceBeg s
    f (SpliceEnd s)     = SpliceEnd s
    k (GsLeft c amt)   = GsLeft c $ fmap g amt 
    k (GsOneLoc c amt) = GsOneLoc c $ fmap g amt 
    k (GsRight c)      = GsRight c
    k2 (CscLeft c amt)  = CscLeft c $ fmap g amt
    k2 (CscRight c) = CscRight c
    

----------------------------------------------------------------------

type MarkLoc a = (Loc,[Mark a])

type MarkLocList a = [MarkLoc a]

{-
matchBrackets :: Map Loc [Mark a] -> Map String [(Loc,Loc)]
matchBrackets marks = M.fromListWith (++) . concatMap f . L.tails
    $ M.toAscList marks
  where
    f :: [MarkLoc a] -> [(String,[(Loc,Loc)])]
    f [] = []
    f ((loc,ms):remain) = map g $ mapMaybe maybeLeftBracket ms
      where
        g :: String -> (String,[(Loc,Loc)])
        g name = let loc2 = searchForRight loc name remain
                 in (name,[(loc,loc2)])


maybeLeftBracket :: Mark a -> Maybe String
maybeLeftBracket (BracketL s) = Just s
maybeLeftBracket _            = Nothing


searchForRight :: Loc -> String -> [MarkLoc a] -> Loc
searchForRight loc name markLocList =
  case L.find (\(_,ms) -> isL name ms || isR name ms) markLocList of
    Nothing -> throwMine $ printf ("error: for BracketL mark at %s, " ++
      "with name '%s', no right bracket mark found") (showLoc2 loc) name
    Just (locR,ms)
      | isR name ms -> locR
      | isL name ms -> throwMine $ printf ("error: when " ++
          "searching for right bracket to match the left " ++
          "bracket mark at %s with name '%s', found another left bracket")
          (showLoc2 loc) name


isL :: String -> [Mark a] -> Bool
isL name marks = not $ null [s | BracketL s <- marks, s==name]

isR name marks = not $ null [s | BracketR s <- marks, s==name]
-}
----------------------------------------------------------------------


-- computeVarMap
--
--   Input: map of Loc to staff name to all RawMarks at that Loc and staff.
--
--   Output: map of Loc to all variable values as defined at that Loc.
--
computeVarMap :: Map Loc (Map String [RawMark a]) ->
                 Map Loc (Map String Double)
computeVarMap mapIn = foldl step M.empty ys
  where
    -- xs: map of Loc to all RawMark on ANY staff at that Loc
    -- xs :: [(Loc,[RawMark a])]
    xs = M.toAscList $ M.map (concat . M.elems) mapIn

    -- ys: for all Locs with a SetVar RawMark, map of Loc to its parameters.
    ys :: [(Loc, (String,NumVar))]
    ys = mapMaybe g xs
    -- g :: (Loc,[RawMark a]) -> Maybe (Loc,(String,NumVar))
    g (loc,marks) = fmap (loc,) . listToMaybe . mapMaybe maybeSetVar $ marks
    maybeSetVar :: RawMark a -> Maybe (String,NumVar)
    maybeSetVar (SetVar s nv) = Just (s,nv)
    maybeSetVar _             = Nothing
    -- 'step': fold step function
    step :: Map Loc (Map String Double) -> (Loc, (String,NumVar)) ->
            Map Loc (Map String Double)
    step vars (locIn,(nameIn,(NumVar x mName))) = M.insert locIn newMap vars
      where
        errMsg l n = throwMine $ printf ("reference to variable at %s " ++
                     "named '%s'---it has not been defined") (showLoc2 l) n
        lastMap :: Map String Double
        lastMap = case M.maxView vars of
          Nothing    -> M.empty
          Just (m,_) -> m
        lookupLastValue n = case M.lookup n lastMap of
            Nothing -> errMsg locIn n
            Just d  -> d
        y = case mName of
          Nothing            -> 1
          Just nameRightSide -> lookupLastValue nameRightSide
        newMap :: Map String Double
        newMap = M.insert nameIn (y*x) lastMap


computeNumVar :: Variables -> Loc -> NumVar -> Double
computeNumVar vars loc (NumVar d mName) = case mName of
  Just s -> case M.lookupLE loc vars of
    Just (_,vs) -> case M.lookup s vs of
      Just x -> d*x
      Nothing -> t (printf "no variable '%s' defined" s)
    Nothing -> t (printf "no variables defined yet")
  Nothing -> d
  where
    t :: String -> Double
    t x = throwMine $ printf "at %s, %s" (showLoc2 loc) x


  
----------------------------------------------------------------------

rawMark :: Parser (RawMark NumVar) 
rawMark = comment <|> setVar <|> (RmMark <$> mark)


comment :: Parser (RawMark NumVar)
comment = try (string "**") >> return Comment

setVar :: Parser (RawMark NumVar) 
setVar = do
  v <- try $ do s <- varName
                char '='
                return s
  n <- numVar
  eof
  return $ SetVar v n



----------------------------------------------------------------------


mark :: Parser MarkN
mark = setTempo <|> arp <|> stac <|> trunc <|> ltrunc <|>
       extend <|> absWarp <|> pause <|> w <|> rampBegin <|> rampEnd <|>
       rampEnd <|> rit <|> accel <|> trillShapeMark <|> tremShapeMark <|>
       bracketL <|> bracketR <|> patch <|> artic <|> boundary <|> adjust <|>
       midiCtrl <|> postPause <|> genericOneLoc <|> genericLeft <|>
       genericRight <|> spliceBeg <|> spliceEnd <|> ctrlSetting


midiCtrl :: Parser MarkN
midiCtrl = do
  try $ do char '*'
           notFollowedBy $ char '*'
  ds <- many1 digit
  char ','
  nv <- numVar
  return $ MidiCtrl (read ds) nv 0
  


setTempo = setTempo1 <|> setTempo2

setTempo1 :: Parser MarkN
setTempo1 = do
  try $ string "T="
  n <- numVar
  flag <- option False (char '*' >> return True)
  eof
  return $ SetTempo Nothing n flag


setTempo2 :: Parser MarkN
setTempo2 = do
  v1 <- try $ do v <- numVar
                 string "=T="
                 return v
  
  v2 <- numVar
  flag <- option False (char '*' >> return True)
  return $ SetTempo (Just v1) v2 flag


arp :: Parser MarkN
arp = do
  try $ string "arp"
  char '@'
  mult <- option 1 (char '-' >> return (-1))
  i <- parseInt
  eof
  return $ ArpDelta $ 0.01*mult*fromIntegral i


stac :: Parser MarkN
stac = do
  try $ string "stac"
  s <- seconds
  eof
  return $ StacDur s


trunc :: Parser MarkN
trunc = do
  try $ string "trn"
  s <- seconds
  eof
  return $ Trunc s


ltrunc :: Parser MarkN
ltrunc = do
  try $ string "ltrn"
  s <- seconds
  eof
  return $ LTrunc s


extend :: Parser MarkN
extend = do
  try $ string "ext"
  s <- seconds
  eof
  return $ Extend s
  

-- beat value can be specified as single int, float, ratio of floats specified
-- with %, and any of that times a variable.
absWarp = 
  (do try $ string "<<"
      -- it's okay here to assume this is an absolute left sided warp
      v <- numVar
      eof
      return $ AbsWarp LeftWarp v)
  <|>
  (do lookAhead $ try $ manyTill (noneOf ">") endsLikeAbsWarp
      v <- numVar
      string ">>"
      eof
      return $ AbsWarp RightWarp v)


endsLikeAbsWarp = try (string ">>" >> eof)


postPause = do
  try $ string "aps"
  v <- numVar
  eof
  return $ PostPause v


pause = do
  try $ string "ps"
  notFollowedBy (char '@') <?> "the @ form of pause no longer accepted"
  v <- numVar
  eof
  return $ Pause v

timeshift = timeshift1 <|> timeshift2

timeshift1 = do
  char '%'
  v <- numVar
  flag <- option False (char '*' >> return True)
  return $ TimeShift Nothing v flag
  
timeshift2 = do
  v1 <- try numVar
  char '%'
  v2 <- numVar
  flag <- option False (char '*' >> return True)
  return $ TimeShift (Just v1) v2 flag

w :: Parser MarkN
w = try (char 'w' >> eof >> return W)


rampBegin = do
  lookAhead $ try $ do many1 (notFollowedBy rampBegMarker >> anyChar)
                       rampBegMarker
                       eof
  r <- numVar
  rampBegMarker
  eof
  return $ RampBeg r


rampEnd = do
  try rampEndMarker
  r1 <- numVar
  mEndBeg <- maybeEndBeg
  case mEndBeg of
    Just (Just r2) -> return $ RampEndBeg r1 r2
    Just Nothing   -> return $ RampEndBeg r1 r1
    Nothing        -> return $ RampEnd r1


--
-- Returns Nothing if this is a ramp end (not an end/begin)
-- Returns (Just x) if this is end/begin, where x is:
--
--   - Nothing if there is only one ratio specified
--   - (Just y) if the second ratio specified is y
--
maybeEndBeg :: Parser (Maybe (Maybe NumVar))
maybeEndBeg = do
  -- This is case of an End/Beg that has split ratios
  (do char '|'
      r <- numVar
      rampBegMarker
      eof
      return $ Just (Just r))
  <|>
  -- This is case of an End/Beg with no second ratio
  (do rampBegMarker
      eof
      return $ Just Nothing)
  <|>
  -- This is the case of a simple ramp end.
  (do eof
      return Nothing)
     
  
rampBegMarker = (try $ string "--") <|> (try $ string "=")


rampEndMarker = (try $ string "--") <|> (try $ string "=")


rit = do
  try $ do string "rit."
           eof
  return RitAccel


accel = do
  try $ string "accel."
  return RitAccel


-- if we get 'tr' followed by ^ or v, okay to assume this is trillShapeMark
trillShapeMark = do
  step1 <- try $ do string "tr"
                    oneOf "^v"
  t <- trillShape step1
  eof
  return $ TrillShapeMark t


tremShapeMark = do
  step1 <- try $ do string "to"
                    choice [char '^', char 'v']
  t <- trillShape step1
  eof
  return $ TremShapeMark t


trillShape :: Char -> Parser TrillShape
trillShape step1 = do
  let seg = do d1 <- digit
               d2 <- digit
               let d = fromIntegral $ read [d1]
               return (scale 0 d 9 4 15,read [d2])
  segs <- sepBy seg (char '-')
  when (null segs) (fail "must be at least one segment in trill/trem shape")
  step2 <- choice [char '^', char 'v']
  let step11 = if step1 == '^' then Upper else Lower
  let step22 = if step2 == '^' then Upper else Lower
  return $ TrillShape step11 segs step22

 
bracketL :: Parser MarkN
bracketL = do
  char '['
  ds <- many1 alphaNum
  mControlName <- optionMaybe (char '#' >> many1 alphaNum)
  vals <- many (char ',' >> numVar)
  eof
  return $ BracketL ds mControlName vals


bracketR :: Parser MarkN
bracketR = do
  lookAhead $ try $ many1 alphaNum >> char ']' >> eof
  ds <- many1 alphaNum
  char ']'
  eof
  return $ BracketR ds


bracketC :: Parser MarkN
bracketC = try $ do
  string "[]"
  eof
  return BracketC


patch :: Parser MarkN
patch = do
  char '$'
  s <- many1 alphaNum
  return $ Patch s


artic :: Parser MarkN
artic = do
  char '#'
  s <- many1 (alphaNum <|> oneOf "#/")
  eof
  return $ Artic s


boundary = boundaryLeadingNumvar <|> simpleBoundary


simpleBoundary :: Parser MarkN
simpleBoundary = do
  char '|'
  eof
  return $ Boundary2 Nothing


boundaryLeadingNumvar :: Parser MarkN
boundaryLeadingNumvar = do
  try $ lookAhead (many1 (noneOf "|") >> char '|' >> eof)
  dir <- (char '-' >> return (-1)) <|> (char '+' >> return 1)
  NumVar x v <- numVar
  return $ Boundary2 (Just $ NumVar (dir*x) v)
  

adjust :: Parser MarkN
adjust = do
  gf <- option True (char '^' >> return False)
  c <- (char '-' >> return (-1)) <|> (char '+' >> return 1)
  v <- numVar
  return $ Adjust2 gf c v


genericOneLoc :: Parser MarkN
genericOneLoc = do
  try $ (char '!' >> notFollowedBy (char '!'))
  typ <- many1 alphaNum
  let oneParam = do char ','
                    numVar
  nvs <- many oneParam
  eof
  return $ GenericShape (GsOneLoc typ nvs) 


genericLeft :: Parser MarkN
genericLeft = do
  char '('
  typ <- many1 alphaNum
  let oneParam = do char ','
                    numVar
  nvs <- many oneParam
  eof
  return $ GenericShape (GsLeft typ nvs)

genericRight :: Parser MarkN
genericRight = try $ do
  typ <- many1 alphaNum
  char ')'
  eof
  return $ GenericShape (GsRight typ) 
    
    
genericCenter :: Parser MarkN
genericCenter = try $ do
  try $ string "!!"
  typ <- many1 alphaNum
  eof
  return $ GenericShape (GsCenter typ)

{-
controlLeft :: Parser MarkN
controlLeft = do
  char '<'
  typ <- many1 alphaNum
  let oneParam = do char ','
                    numVar
  nvs <- many oneParam
  eof
  return $ ControlShape (CscLeft typ nvs)

controlRight :: Parser MarkN
controlRight = try $ do
  typ <- many1 alphaNum
  char '>'
  eof
  return $ ControlShape (CscRight typ)
      
controlCenter :: Parser MarkN
controlCenter = try $ do
  try $ string "&&"
  typ <- many1 alphaNum
  eof
  return $ ControlShape (CscCenter typ)
  
-}

spliceBeg :: Parser MarkN
spliceBeg = do
  try (string "//")
  n <- many1 alphaNum
  eof
  return $ SpliceBeg n 


spliceEnd :: Parser MarkN
spliceEnd = do
  try $ char '/' >> notFollowedBy (char '/')
  n <- many1 alphaNum
  eof
  return $ SpliceEnd n


ctrlSetting :: Parser MarkN
ctrlSetting = do
  char '&'
  n <- many1 alphaNum
  eof
  return $ CtrlSetting n


----------------------------------------------------------------------

seconds = do 
  char '@'
  i <- parseInt
  return $ 0.01 * fromIntegral i


numVar :: Parser NumVar
numVar = 
  (do n <- varName 
      return $ NumVar 1 (Just n))
  <|>
  (do n <- num
      v <- optionMaybe varName
      return $ NumVar n v)


varName :: Parser String
varName = do
  c  <- lower
  cs <- many alphaNum
  return $ c:cs


-- num examples
--
-- 3          integer: 3
-- 110        integer: 110
-- 3.1        float: 3.1
-- 100.3      float with 2 or more digits: 100.3 (not interpreted as percent)
-- 2:3        2/3
-- 10:12      10/12
-- 1.3:1.4    1.3/1.4  (can have ratio of floats)
-- :3         1/3
-- :12        1/12
num :: Parser Double
num =
  -- :float
  (do divChar
      d <- simpleDouble
      return (if d /= 0 then 1/d else error "division by zero"))
  <|>
  -- float not followed by : (the case of int not followed by : will have been
  -- caught above
  (try $ do d <- simpleDouble
            notFollowedBy $ divChar
            return d)
  <|>
  -- float:float
  (try $ do d1 <- simpleDouble
            divChar
            d2 <- simpleDouble
            return $ d1/d2)


divChar = char '%' <|> char ':'


simpleDouble :: Parser Double
simpleDouble = do
  s <- simpleDouble_dot <|> simpleDouble_notDot
  case reads s of
    (f,[]):_ -> return f
    _        -> fail "malformed numeric value"


simpleDouble_dot :: Parser String
simpleDouble_dot = do
  char '.'
  s <- many1 (oneOf "0123456789")
  return $ "0." ++ s


simpleDouble_notDot :: Parser String
simpleDouble_notDot = many1 $ oneOf ".0123456789"  


----------------------------------------------------------------------
----------------------------------------------------------------------
--                  marks positioned below the staff

-- level2 dynamcs:
--
--   don't use T to separate halves of left-right


commonLookAhead :: Char -> Parser String
commonLookAhead c = lookAhead $ try $ manyTill (noneOf [c]) (char c)


dynamic = lev1DynL <|> lev1DynR <|> lev2DynL <|> lev2DynLR <|> lev2DynR <|>
          lev1DynLR1 <|> lev1DynLR2


lev1DynL :: Parser (Mark NumVar)
lev1DynL = try $ do
  notFollowedBy (char '*')
  commonLookAhead '*'
  v <- rawDynNum
  char '*'
  eof
  return $ Lev1DynL v


lev1DynR :: Parser (Mark NumVar) 
lev1DynR = try $ do
  char '*'
  v <- rawDynNum
  eof
  return $ Lev1DynR v

lev1DynLR1 :: Parser (Mark NumVar)
lev1DynLR1 = try $ do
  char '*'
  v <- rawDynNum
  char '*'
  eof
  return $ Lev1DynLR v v


lev1DynLR2 :: Parser (Mark NumVar)
lev1DynLR2 = try $ do
  char '*'
  v1 <- rawDynNum
  char ','
  v2 <- rawDynNum
  char '*'
  eof
  return $ Lev1DynLR v1 v2
  

rawDynNum :: Parser Double
rawDynNum = do
  i <- digit
  return $ fromIntegral $ read [i]

{-
dynamic2 :: Parser (Mark NumVar)
dynamic2 = do
  lookAhead $ try $ manyTill (noneOf "T") (char 'T')
  d1 <- rawDyn
  char 'T'
  d2 <- rawDyn
  ramp <- option False (char '=' >> return True)
  eof
  return $ DynMark (Just d1) d2 ramp 
-}

rawDyn :: Parser Double
rawDyn = (try (string "pppp") >> return 0)
      <|> 
      (try (string "ppp") >> return 1)
      <|>
      (try (string "pp") >> return 2)
      <|>
      (try (string "p") >> return 3)
      <|>
      (try (string "ffff") >> return 9)
      <|>
      (try (string "fff") >> return 8)
      <|>
      (try (string "ff") >> return 7)
      <|>
      (try (string "f") >> return 6)
      <|>
      (try (string "mp") >> return 4)
      <|>
      (try (string "mf") >> return 5)


lev2DynR :: Parser (Mark NumVar)
lev2DynR = try $ do
  char '*'
  sign <- (char '+' >> return 1) <|> (char '-' >> return (-1))
  v <- numVar
  eof
  return $ Lev2DynR sign v


lev2DynL :: Parser (Mark NumVar)
lev2DynL = try $ do
  notFollowedBy $ char '*'
  commonLookAhead '*'
  sign <- (char '+' >> return 1) <|> (char '-' >> return (-1))
  v <- numVar
  char '*'
  eof
  return $ Lev2DynL sign v


lev2DynLR :: Parser (Mark NumVar)
lev2DynLR = try $ do
  char '"'
  sign <- (char '+' >> return 1) <|> (char '-' >> return (-1))
  v <- numVar
  eof
  return $ Lev2DynLR sign v
