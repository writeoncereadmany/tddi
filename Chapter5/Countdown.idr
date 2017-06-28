module Main

import System
import Data.String

readNumber : IO (Maybe Nat)
readNumber = do input <- getLine
                pure (parsePositive input)

countdown : (secs: Nat) -> IO ()
countdown Z = putStrLn "Lift off!"
countdown time@(S secs) = do putStrLn (show time)
                             usleep 1000000
                             countdown secs

countdowns : IO ()
countdowns = do putStrLn "Enter countdown size"
                Just start <- readNumber
                | Nothing => do putStrLn "Invalid input"
                                countdowns
                countdown start
                putStrLn "Another? (y/n)"
                response <- getLine
                if response == "y" then countdowns else pure ()
