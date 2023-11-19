
import qualified Data.ByteString as B
import Translation.ParseConfig
import Data.Char

{-
s1 =  unlines [ "[pattern [dynamics 2 0 0]" 
              , "         [tempo    2 1 1]"
              , "         [tempo    3 1 1 1]"
              , "]"
              ]


showPatternDescr = show

main = case parse pattern "" (B.pack $ map (fromIntegral . ord) s1) of
  Left err -> print err
  Right (CSPattern p)  -> putStrLn $ showPatternDescr p
-}

main = do
  b <- readFile "config1.cfg"
  print $ parseConfig b

