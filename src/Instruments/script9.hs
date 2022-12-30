import Util.Math


main = do
  let s = scale_3_2 23 31 39 (log 69.9) (log 166.8)
  print $ exp s
