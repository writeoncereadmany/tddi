module Main

import Data.String
import System

data IsValid = Valid | Invalid

readNumber : IO (Maybe Nat)
readNumber = getLine >>= (pure . parsePositive)

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses = do putStrLn ("Guesses taken: " ++ show guesses)
                          putStrLn "Enter your guess: "
                          nextGuess <- readNumber 
                          case map (compare target) nextGuess of
                            Nothing => do fail "Invalid input: try again!" Invalid
                            (Just LT) => do fail "Too high!" Valid
                            (Just GT) => do fail "Too low!" Valid
                            (Just EQ) => do putStrLn "Correct! You win!"
                                            pure ()
                       where
                         fail : String -> IsValid -> IO ()
                         fail message isValid = do putStrLn message
                                                   let newGuesses = case isValid of
                                                     Valid => guesses + 1
                                                     Invalid => guesses
                                                   guess target newGuesses

main : IO ()
main = do seconds <- time
          let target = mod seconds 100
          guess (cast target) 0
