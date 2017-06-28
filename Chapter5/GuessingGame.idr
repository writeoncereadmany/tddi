module Main

import Data.String

readNumber : IO (Maybe Nat)
readNumber = do input <- getLine
                pure (parsePositive input)

guess : (target : Nat) -> IO ()
guess target = do putStrLn "Enter your guess: "
                  nextGuess <- readNumber
                  case map (compare target) nextGuess of
                    Nothing => do fail "Invalid input: try again!"
                    (Just LT) => do fail "Too high!"
                    (Just GT) => do fail "Too low!"
                    (Just EQ) => do putStrLn "Correct! You win!"
                                    pure ()
               where
                    fail : String -> IO ()
                    fail message = do putStrLn message
                                      guess target
