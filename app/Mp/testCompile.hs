
import System.IO
import System.Process
import System.FilePath
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
import Instruments.SendControl
-- import Language.Haskell.Interpreter


main = compile "calib"
  

compile :: FilePath -> IO Bool
compile fp = do
  --
  let c = printf "ghc --make -i/Users/Mike/Dropbox/stack/cac/src %s" fp
  -- let c = printf "stack ghc --stack-yaml $DB/stack/cac/stack.yaml --package cac2 %s" fp
  (_,_,_,h) <- createProcess (shell c)
  exit <- waitForProcess h
  putStrLn ""
  return $ exit == ExitSuccess

    
