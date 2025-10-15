import System.Directory
import System.FilePath

main = do
  let p = "/Users/mike/Dropbox/music/comp/2025/rapid"
  cs <- listDirectory p
  print cs
