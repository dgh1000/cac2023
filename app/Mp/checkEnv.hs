import System.Environment

main = do
  s <- lookupEnv "MUSIC_SYSTEM"
  print s
