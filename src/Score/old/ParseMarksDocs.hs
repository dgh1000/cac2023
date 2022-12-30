{-# LANGUAGE TupleSections #-}

module Score.ParseMarks where


import qualified Data.Map as M
import qualified Data.List as L
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

tempos in setting tempo or variable

  *** we no longer allow setting tempo in terms of prevailing tempo except in
      ramps. you can only give a number, or a number times a variable, in the
      T= form ***

  *** Q is no longer necessary. it is assumed that all tempo specifications
      are in number of quarters per minute. Later maybe we can add some other
      beat type ***


  T=100  (Q is no longer necessary)

    set tempo to 100 quarters per minute

  T=a

    set tempo to number of beats in variable a

  T=9%10a

    set tempo to 9/10 of a


variables to indicate a number of beats

  a=90

    set a to 90 quarters

  a=3%2b

    set variable a to 3/2 variable b

  a=3%2

    set a to 3/2 quarters

ramps

  Update June 2016: simplifying: ramp tempos will always be ratios with
  respect to prevaling tempo. Also ratios can be specified by omitting either
  number and the missing number is considered to be 1. Or a single int X means
  X:1. And a "vertical pipe character" divides the ratios at a sudden tempo
  change.

  Also update June 2016: changing ramp indicator to two dashes, not greater
  than/ less than symbols.

  Further change: now single = will be accepted as ramp indicator character

    (Legacy form supported for now.)

  In the following we call the prevailing tempo P

  3:2=    =1:2

    ramp from 3/2 P to 1/2 P. return to P afterward.

  1=   =2=    =4:5

    ramp from P to 2 P, then back to 4/5 P, and return to P

  1=   =1:2|4:5=  =1

    ramp from P to 1/2 P, then sudden change to 4:5 P, ramping back up to P

warps

  note: in warps with interval width specified, it can be an integer (not
  necessarily a ratio) -- however the amount of the warp has to be a ratio

  <1;:10 

     global warp because it has no S. Left-sided warp because < is on the
     left. warp interval of 1 beat because "1" is before the ";". Compresses
     interval because < is pointing in same direction as side it is on. Total
     compression amount is 1/10 of a beat.

  <+0.34<

     staff-only (because of the +), double-sided warp because there are '<' on
     each side. There is only one number inside the warp, so that number is
     the amount to compress left interval and stretch right interval, while
     look for w's to mark ends of interval

  <+1;:10<

     staff-only double-sided, left and right intervals are each 1 beat in
     duration

  <+1;%10

     staff-only single-sided. This will cause staves to get out of sync until
     the next one sided warp staff-only warp in the same warp that restores
     sync


  >%10> 

     global double-sided, stretch left interval and compress right interval by
     1/10 of a beat. look for w's to define intervals.

  3%2;1.6>

     global right-sided warp. The arrow pointing right means compress the
     interval. This doesn't actually affect the arrival time at the location
     the warp is marked but rather makes the next 3/2 of a beat speed up by a
     total amount of 1/8 a beat.
  

new kind of warp: to absolute final duration

  --> must use w marker

  --> called "absolute warp"

  <<1

    warp time so that duration between left w and this mark is 1 quarter

  1.2>>

    duration between this mark and right w should be 1.2 quarters

pause

  ps5 : NOT SUPPORTED ANY MORE

    legacy form: pause for 50 milliseconds

  ps@5

    new form: pause for 50 milliseconds

  ps%8

    pause for 1/8 of a quarter

  ps2%7

    pause for 2/7 of a quarter

  ps1.5

    pause for 1.5 quarters

arpeggio and staccato durations

  arp10 : NOT SUPPORTED ANY MORE

    legacy form: set arp delta to 10*10 milliseconds

  stac5 : NOT SUPPORTED ANY MORE

    legacy form: set staccato duration to 5*10 milliseconds

  arp@5, stac@7

    updated form of specifying 10's of milliseconds


rit/accel

  either "rit." or "accel."

    either of these specifies a ramp from prevailing tempo to next marked
    tempo. it treats "rit." and "accel." the same; the next marked tempo
    determines whether the tempo will speed up or slow down


splice

  $a

    mark beginning of sequence of segments, each one marked by a letter such
    as $b, $c etc. 


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


macros: 

  definition:

    name must start with letter, followed by 0 or more alphaNum

    arguments are specified by /<digit> and must be numbered consecutively

    example:

      wl{<+%2;%/1<}

  macro invocation: give name and arguments separated by !

    !wl!12

complex pauses

    multiple pause

      mp3/1/:6  --   mp<# pauses>/<beat separation of pauses>/<pause dur>



        insert pause of 1/6 quarters at current location, but also insert
        pauses of smaller duration in two previous locations (3 locations
        total), 1 beat before and 2 beats before. Pause lengths are scaled
        as arithmetic progression

    rit pause

      

      rp.95|1.1/1/:6    --   rp

        insert pause of :6 quarters. slow down for 1 beat prior to the
        pause to 95 percent of prevailing tempo. after the pause, tempo will
        be 110 percent of prevailing tempo and then slow down to prevailing
        tempo for 1 beat.

staff adjusts

  ^+:20 -- from left ^ to right ^ forms two sections. rit or accel left side
           in order to achive the specified delay (:20 of a beat). then
           adjust right side linearally to achieve overall same time

  ^^-:20 -- same as above, but global, and move arrival time earlier

  ^+:2;1 -- delay by half a quarter, with width of 1 quarter (basically
            assuming a ^ 1 beat to the left and a ^ one beat to the right)

rits then accel

   something equivalent to 1=   =0.95|1.05=   =1

   do it with              r         ra.95         r

   and have total time across span be unaffected

dynamic shapes

  <>.70/.50

    marks beginning of phrase that continues to <>+ mark. crescendo to an
    additional 50% dynamic step, then descrendo to original dynamic. peak of
    crescendo will occur 70% of way through shape

  <>

    marks end of <> shape

  ><.70/.50

    same as <> but descrendo follow by crescendo

  ><
 
    end of >< shape


bracketed areas

  [abc   abc]    : bracketed area of name "abc"
  [ab:2  ab]     : parameter passed to bracket


instrument technique

  some words are recognized as being instrument technique:

    arco, pizz., DXF

"two modify"

  tm[1=.9^][^/.9=1]n1

    two region modify, from t marker on left side to t marker on right side,
    with TempoModify's that ramp from 1 to 0.9, and on the other side, the
    multiplicative inverse of 0.9 to 1. left one is scaled parabolically in
    which fastest changing side is on the right. other one is scaled
    parabolically withi fastest changing side on the left. the final n1 means
    the final duration is normalized 90% of the way back toward original
    duration

patch: a general term for regions of sound within a staff

  different regions may be translated into MIDI differently

  $p:name


time region elements

  two kinds of marks: (1) Boundary (2) Alter1

    Boundary defines points in time which should not be changed during Pass 1
    of time alteration

    Alter1 defines alterations in arrival time. these are created in such a
    way that boundary times are not altered (so music before the boundary is
    sped up, and music after is slowed down)

    | is a boundary

    +:4| is a boundary, in which only last slice before boundary is altered

    

  we define a time region as a span of time in which we are altering time but
  maintaining overall difference in time between two points

  the span between two | should stay the same. in-between will appear +:20 and
  so forth which means delay arrival by 20th of a beat. special mark +:20|
  means delay the arrival entirely in the last slice before that point.

  it is not valid to type -:20| (- sign is not valid in boundary symbol)

  0     1     2     3     4     5     6     7     8     9
  |                                               +:8|
  

  



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
    f loc s = case parse (deltaDyn <|> dynamic) "" s of
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
    f (BracketL s v) =  BracketL s (fmap g v)
    f (BracketR s) =  BracketR s
    f (Boundary2 nv) =  Boundary2 $ fmap g nv
    f (Adjust2 fl d nv1) =  Adjust2 fl d (g nv1)
    f (MidiCtrl cn nv1 _) = MidiCtrl cn (g nv1) (round $ g nv1)
    f (GenericShape c) = GenericShape (k c)
    k (GsLeft c) = GsLeft c
    k (GsCenter c amt) = GsCenter c $ fmap g amt
    k (GsRight c) = GsRight c
      

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
       midiCtrl <|> postPause <|> genericCenter <|> genericLeft <|>
       genericRight


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
  value <- optionMaybe numVar
  eof
  return $ BracketL ds value 


bracketR :: Parser MarkN
bracketR = do
  lookAhead $ try $ many1 alphaNum >> char ']' >> eof
  ds <- many1 alphaNum
  char ']'
  eof
  return $ BracketR ds


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


genericCenter :: Parser MarkN
genericCenter = do
  char '!'
  a <- alphaNum
  nv <- optionMaybe numVar
  eof
  return $ GenericShape (GsCenter a nv) 


genericLeft :: Parser MarkN
genericLeft = do
  char '('
  a <- alphaNum
  eof
  return $ GenericShape (GsLeft a)


genericRight :: Parser MarkN
genericRight = do
  char ')'
  a <- alphaNum
  eof
  return $ GenericShape (GsRight a)






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
      return $ 1/d)
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

deltaDyn :: Parser (Mark NumVar)
deltaDyn = do
  sign <- (char '+' >> return 1) <|> (char '-' >> return (-1))
  nv <- numVar
  return $ DeltaDyn sign nv


dynamic1 :: Parser (Mark NumVar)
dynamic1 = do
  v <- dyn
  ramp <- option False (char '*' >> return True) 
  return $ Dynamic v ramp


dynamic2 :: Parser (Mark NumVar)


dyn :: Parser Double
dyn = (try (string "ppp") >> return 1)
      <|>
      (try (string "pp") >> return 2)
      <|>
      (try (string "p") >> return 3)
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

