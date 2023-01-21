import Text.Printf
import Sound.PortMidi

showMidiDevice :: Int -> IO ()
showMidiDevice x = do
  di <- getDeviceInfo x
  putStrLn $ printf "%d: %s '%s'" x (decideInputOrOutput di) (name di) 


decideInputOrOutput di = if input di then "Input :" else "Output:"


main = do
  c <- countDevices
  mapM_ showMidiDevice [0..c-1]

