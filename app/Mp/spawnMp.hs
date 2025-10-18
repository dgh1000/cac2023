
import Data.List ( sort )
import System.Environment
import System.IO
import System.Process
import System.FilePath
import System.Directory
import System.Console.Haskeline
import GHC.IO.Exception
import Control.Monad
import Control.Monad.IO.Class
import Control.Concurrent
import Debug.Trace
import Text.Printf
import Text.Parsec
import Text.Parsec.String
import Util.FileUtil
-- import Translation.SendControl
import System.Directory (exeExtension)
import GHC.IO (unsafePerformIO)
-- import Language.Haskell.Interpreter


-- main :: IO ()
-- main = findXmlInDocDir "/Users/mike/Dropbox/music/comp/2025/dorico/project-01.dorico"

main :: IO ()
main = runInputT defaultSettings $ loop Nothing


loop :: Maybe ProcessHandle -> InputT IO ()
loop mProcHand = do
  mL <- getInputLine "spawn > "
  case mL of
    Nothing -> loop mProcHand
    Just l  -> do
      (flag,mPh) <- liftIO $ loopIO l mProcHand
      if flag then return () else loop mPh


-- prepareRun
--
--   prepare run:
--
--     (1) locate the most recent sib file and compile the associated hs
--         file
--
--     (2) stop any running previous compiled process
--
--   Returns Nothing if there was a compilation error, and <Just filename>
--   where <filename> is the name of most recent sib file without any
--   extension.
-- 

loopIO :: String -> Maybe ProcessHandle -> IO (Bool,Maybe ProcessHandle)
loopIO l mProcHand = do
  case parse cmdLine "" l of
    Left err -> putStrLn ("parse error: " ++ show err)
                  >> return (False,mProcHand)
    Right cmd -> case cmd of
      Play (beg,mEnd) mSolo mSplice -> do
        result <- prepareRun mProcHand (RoPlay beg mEnd mSolo mSplice)
        return (False,result)
      Calib -> do
        result <- prepareCalib mProcHand
        return (False,result)
      SendCtrl str chan crtl val -> error "foo" {- do
        result <- prepareRun mProcHand (RoSendCtrl str chan crtl val)
        return (False,result) -}
      SendCtrlFile strNum -> error "foo" {- do
        doStop mProcHand >> return (False,Nothing)
        threadDelay 300000
        doSendParsedControls strNum
        return (False,Nothing) -}
      Quit  -> doStop mProcHand >> return (True,Nothing)
      Stop  -> doStop mProcHand >> return (False,Nothing)
      Blank -> return (False,mProcHand)
      

-- prepareRun
--

compDirectory :: IO FilePath
compDirectory = do
  e <- lookupEnv "COMPUTER_SYSTEM"
  case e of
    Just _  -> return "/Users/Mike/Dropbox/music/comp"
    Nothing -> return "c:\\Users\\micha\\Dropbox\\music\\comp"


homeDirectory :: IO FilePath
homeDirectory = do
  e <- lookupEnv "COMPUTER_SYSTEM"
  case e of
    Just _  -> return "/Users/Mike/"
    Nothing -> return "c:\\Users\\micha\\"


isMusicDocName f = e == ".sib" || e == ".mscz" || e == ".dorico"
  where
    e = takeExtension f

-- Oct 2025, moving to Dorico. It exports the MusicXML without giving
-- a chance to choose filename. Instead we will probably look for an xml file
-- name beginning with music doc name and rename it to just the music
-- doc name plus ".musicxml"
prepareRun :: Maybe ProcessHandle -> RunOnceArg -> IO (Maybe ProcessHandle)
prepareRun mProcHand arg = do
  compD <- compDirectory
  treeFs <- treeFiles isMusicDocName compD
  fp <- mostRecentFileL treeFs 
  let fpNoExt = dropExtension fp
  findXmlInDocDir fpNoExt
  prepareInXml fpNoExt
  flag <- compile (fp -<.> "hs")
  if flag
    then do
      doStop mProcHand
      ph <- spawn (fpNoExt ++ exeExtension) arg
      return $ Just ph
    else return mProcHand

-- After moving to Dorico: 
-- we need to write a function to rename
-- its exported musicxml file to a name that matches the base
-- name of the music doc file plus ".musicxml"
--
-- baseNameDoc: full path to the music doc minus its extension
findXmlInDocDir :: FilePath -> IO ()
findXmlInDocDir pathBaseNameDoc = do
  -- we need to search directory for file that begins with baseNameDoc
  -- and has a .musicxml extension
  let dir = takeDirectory pathBaseNameDoc
      baseName = takeBaseName pathBaseNameDoc
      l = length baseName
      willConsider p = cond1 && cond2 && cond3
        where 
          cond1 = take l (takeBaseName p) == baseName
          cond2 = takeExtension p == ".musicxml" 
          cond3 = length (takeBaseName p) /= length baseName
  considered <- map (dir </>) . filter willConsider <$> listDirectory dir
  fn <- mostRecentXml considered
  putStrLn $ "findXmlInDocDir: Files considered for musicxml copy: " ++ show considered
  let file1 = fn
      file2 = dir </> baseName ++ ".musicxml"
  if file1 == file2 
    then printf "No need to copy, doc name + .musicxml is most recent"
    else do
      putStrLn $ printf "findXmlInDocDir: copying %s to %s" file1 file2
      copyFile file1 file2

  {-
  case considered of
    []  -> "findXmlInDocDir: no xml file in doc directory to rename" `trace` return ()
    [x] -> do
      let file1 = dir </> x
          file2 = dir </> baseName ++ ".musicxml"
      putStrLn $ printf "findXmlInDocDir: copying %s to %s" file1 file2
      copyFile file1 file2
    _   -> error "2 xml files in doc dir with matching name prefix"
  -}

mostRecentXml :: [FilePath] -> IO FilePath
mostRecentXml fps = do
  times <- mapM getModificationTime fps
  let tuples = sort $ zip times fps
      (_,out) = case tuples of
        [] -> error ("in mostRecentXml, no files were passed in. This could indicate there are no XML " ++
              "files in music doc directory")
        xs -> last xs
  return out

prepareInXml :: FilePath -> IO ()
prepareInXml fp = do
  let localXmlFp = fp <.> "musicxml"
  homeD <- homeDirectory
  let outXmlFp = homeD </> "out.musicxml"
  -- if either does not exist, use the other
  -- if both exist use the most recent one
  localXmlExists <- doesFileExist localXmlFp
  outXmlExists <- doesFileExist outXmlFp
  which <- case (localXmlExists,outXmlExists) of
    (True,True) -> do
      t1 <- getModificationTime localXmlFp
      t2 <- getModificationTime outXmlFp
      if t1 > t2 then return 1 else return 2
    (True,False) -> return 1
    (False,True) -> return 2
    _            -> error "neither out.musicxml nor local musicxml files exist"
  if which == 1 
    then "using local XML file" `trace` copyFile localXmlFp $ homeD </> "in.musicxml"
    else copyFile outXmlFp $ homeD </> "in.musicxml"


prepareCalib :: Maybe ProcessHandle -> IO (Maybe ProcessHandle)
prepareCalib mProcHand = do
  let fpNoExt = "/Users/Mike/Dropbox/stack/cac/app/CalibrateSampler/calib"
  flag <- compile $ fpNoExt <.> "hs"
  if flag
    then do
      doStop mProcHand
      ph <- spawnNoArg fpNoExt
      return $ Just ph
    else return mProcHand

{-
prepareCalib :: Maybe ProcessHandle -> IO (Maybe ProcessHandle)
prepareCalib mProcHand = do
  let baseName = "/Users/Mike/Dropbox/stack/cac/app/CalibrateSampler/calib"
  flag <- compile $ baseName ++ ".ns"
  if flag
    then do
      doStop mProcHand
      ph <- spawn baseName
      return $ Just ph
    else return mProcHand
-}

doStop :: Maybe ProcessHandle -> IO ()
doStop mProcHand = case mProcHand of
  Nothing -> return ()
  Just h -> do
    c <- getProcessExitCode h
    case c of
      Just _ -> "process already finished, not interrupting" `trace` return ()
      Nothing -> "process needs to be interrupted" `trace` interruptProcessGroupOf h >> return ()
  
 
-- printf "ghc --make -i/Users/Mike/Dropbox/stack/cac/src -package mtl %s" fp
cacSrc :: IO FilePath
cacSrc = do
  e <- lookupEnv "COMPUTER_SYSTEM"
  case e of
    Just _  -> return "/Users/Mike/Dropbox/stack/cac/src"
    Nothing -> return "c:\\Users\\micha\\Dropbox\\stackw\\cac\\src"



compile :: FilePath -> IO Bool
compile fp = do
  srcPath <- cacSrc
  let c = printf "ghc --make -i%s -package mtl %s" srcPath fp
  -- let c = printf "stack ghc --stack-yaml $DB/stack/cac/stack.yaml --package cac2 %s" fp
  (_,_,_,h) <- createProcess (shell c)
  exit <- waitForProcess h
  putStrLn ""
  return $ exit == ExitSuccess

    
spawn :: FilePath -> RunOnceArg -> IO ProcessHandle
spawn execName cmd = do
  let cp = CreateProcess (RawCommand execName [showArgData cmd])
        Nothing Nothing
        CreatePipe Inherit Inherit -- std_in std_out std_err
        False  -- close_fds
        False  -- create_group
        True   -- delegate_ctlc
        False  -- detach_console
        False  -- create_new_console
        False  -- new_session
        Nothing -- child_group
        Nothing -- child_user
        False   -- use_process_jobs
  (_,_,_,ph) <- createProcess cp
  return ph


spawnNoArg :: FilePath -> IO ProcessHandle
spawnNoArg execName = do
  let cp = CreateProcess (RawCommand execName [])
        Nothing Nothing
        CreatePipe Inherit Inherit -- std_in std_out std_err
        False  -- close_fds
        False  -- create_group
        True   -- delegate_ctlc
        False  -- detach_console
        False  -- create_new_console
        False  -- new_session
        Nothing -- child_group
        Nothing -- child_user
        False   -- use_process_jobs
  (_,_,_,ph) <- createProcess cp
  return ph
  



spawnCtrl str chan ctrl = do
  let execName = "/Users/Mike/Dropbox/stack/cac/src/Midi/sendCtrl"
  callProcess execName [printf "%d:%d:%d" str chan ctrl]


spawnCalib str chan ctrl = do
  let execName = "/Users/Mike/Dropbox/stack/cac/app/Calib/calib"
  callProcess execName []

  
----------------------------------------------------------------------
--                    parsing command line




data RunOnceArg = RoPlay Int (Maybe Int) (Maybe String) (Maybe String)
                     -- <beg msr> <maybe end msr> <maybe solo meta>
                     -- <maybe splice point>
                | RoSendCtrl Int Int Int Int


-- example: "p2-3{}{}"
showArgData (RoPlay mBeg mEnd mSolo mSplice) =
  printf "p%d-%d{%s}{%s}"
         mBeg
         (case mEnd    of {Nothing -> 0;  Just m -> m})
         (case mSolo   of {Nothing -> ""; Just s -> s})
         (case mSplice of {Nothing -> ""; Just s -> s})
showArgData (RoSendCtrl a b c d) =
  printf "c%d:%d:%d:%d" a b c d
-- showArgData (RoSendCtrlSet a b) = printf "t%d:%d" a b


data CmdLine = Play (Int,Maybe Int) (Maybe String) (Maybe String)
                 -- 
             | SendCtrl Int Int Int Int  -- stream, chan, ctrl, value
             | SendCtrlFile Int -- stream, set #
             | Calib
             | Quit
             | Stop
             | Blank
             | Redo


cmdLine :: Parser CmdLine
cmdLine = play <|> calib <|> quit <|> stop <|> blank <|> redo <|> sendCtrl <|>
          sendCtrlSet


play = do
  ds1 <- many1 digit
  ds2 <- optionMaybe $ char '-' >> many1 digit
  many space
  mSplice <- optionMaybe $ do char '/'
                              many1 alphaNum
  soloMeta <- optionMaybe $ between (char '[') (char ']')
                                     (many1 $ noneOf "[]")
  return $ Play (read ds1,read <$> ds2) soloMeta mSplice

quit = char 'q' >> eof >> return Quit

stop = char 's' >> eof >> return Stop

blank = eof >> return Blank

redo = char 'r' >> eof >> return Redo

calib = char 'b' >> eof >> return Calib


sendCtrl = do
  try $ string "c"
  many1 space
  strNum  <- many1 digit
  many1 space
  chanNum <- many1 digit
  many1 space 
  ctrlNum <- many1 digit
  many1 space
  val <- many1 digit
  eof
  return $ SendCtrl (read strNum) (read chanNum) (read ctrlNum) (read val)


sendCtrlSet = do
  char 't'
  many1 space
  strNum <- many1 digit
  many space
  eof
  return $ SendCtrlFile (read strNum)
  
  

----------------------------------------------------------------------

