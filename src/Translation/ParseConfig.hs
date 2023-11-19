module Translation.ParseConfig where

import Text.Parsec
import Text.Parsec.String
import Translation
import qualified Data.Map as M
import Util.Exception
import Debug.Trace


parseConfig :: String -> Either String MidiConfigFile
parseConfig s = case parse parseFile "" s of
  Left err -> Left $ show err
  Right v  -> Right v


parseFile :: Parser MidiConfigFile
parseFile = MidiConfigFile . M.fromList <$> many midiConfig


parseFile2 :: Parser MidiConfigFile
parseFile2 = do
  m <- manyTill midiConfig eof
  eof
  return $ MidiConfigFile (M.fromList m)


midiConfig :: Parser (String,MidiConfig)
midiConfig = do
  many ignored
  n <- midiConfigName
  many ignored
  cs <- many1 midiConfigOneDest
  many ignored
  string "EndName"
  many ignored
  return (n,MidiConfig cs)


ignored = many1 space <|> comment


comment1 = do
  char '{'
  manyTill anyChar (char '}')


comment2 = do
  char '['
  manyTill anyChar (char ']')

comment :: Parser String
comment = comment1 <|> comment2

midiConfigName :: Parser String
midiConfigName = do
  try $ string "Name:"
  manyTill anyChar ignored

midiConfigOneDest :: Parser MidiConfigOneDest
midiConfigOneDest = do
  try $ string "Dest:"
  (stream,chan) <- twoNums "stream" "chan"
  many ignored
  cs <- many1 oneControl
  string "EndDest"
  many ignored
  return $ MidiConfigOneDest (stream,chan) cs


oneControl :: Parser (Int,Int)
oneControl = do
  try $ string "Control:"
  (ctrlNum,value) <- twoNums "ctrlNum" "value"
  many ignored
  return (ctrlNum,value)


twoNums :: String -> String -> Parser (Int,Int)
twoNums type1 type2 = do
  num1S <- manyTill digit (char ',')
  let num1 = case reads num1S of
        (i,_):_ -> i
        _       -> throwMine $ "in parser midiConfigOneDest, " ++
                    " no valid " ++ type1 ++ " integer"
  num2S <- manyTill digit ignored
  let num2 = case reads num2S of
        (i,_):_ -> i
        _       -> throwMine $ "in parser midiConfigOneDest, " ++
                    " no valid " ++ type2 ++ " integer"
  return (num1,num2)
