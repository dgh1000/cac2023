
import Text.Printf
import System.MIDI

-- | Prints all available MIDI destination names to the console
printDestinationNames :: IO ()
printDestinationNames = do
    -- 1. Fetch the list of Destination handles from the OS layer
    destinations <- enumerateDestinations
    
    -- 2. Extract the string name from each individual Destination handle
    names <- mapM getName destinations
    
    -- 3. Print out the structured results
    putStrLn "Available MIDI Destinations:"
    mapM_ putStrLn names
