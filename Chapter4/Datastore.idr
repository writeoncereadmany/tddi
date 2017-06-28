module Main

import Data.Vect
import Data.String

data DataStore : Type where
  MkData : (size : Nat) ->
           (items : Vect size String) ->
           DataStore

total
size : DataStore -> Nat
size (MkData size' _) = size'

total
items : (store : DataStore) -> Vect (size store) String
items (MkData _ items') = items'

total
addToStore : DataStore -> String -> DataStore
addToStore (MkData size items) newItem = MkData _ (addToData items)
  where
    addToData : Vect old String -> Vect (S old) String
    addToData [] = [newItem]
    addToData (x :: xs) = x :: addToData xs

data Command = Add String
             | Search String
             | Get Integer
             | Size
             | Quit

total
joinString : (separator : String) -> (strings : List String) -> String
joinString separator [] = ""
joinString separator [x] = x
joinString separator (x :: y :: xs) = x ++ separator ++ joinString separator (y :: xs)

total
parseCommand : (command : String) -> (arg : String) -> Maybe Command
parseCommand "add" arg = Just (Add arg)
parseCommand "search" arg = Just (Search arg)
parseCommand "get" arg = let number = parseInteger arg
                         in map Get number
parseCommand "size" _  = Just Size
parseCommand "quit" _  = Just Quit
parseCommand _ _ = Nothing

total
parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
  (command, arg) => parseCommand command (ltrim arg)

total
getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store =
  let storedItems = items store
  in case integerToFin pos (size store) of
    Nothing => Just ("Out of range\n", store)
    (Just i) => Just (index i storedItems ++ "\n", store)

total
withIndices : Vect n elem -> List (Nat, elem)
withIndices { n } xs = zip [1..n] (toList xs)

total
searchStore : (store : DataStore) -> (substring : String) -> Maybe (String, DataStore)
searchStore store substring =
  let indexed = withIndices (items store)
      matching = filter ((isInfixOf substring) . snd) indexed
      formatted = map (\(i, val) => show i ++ ": " ++ val) matching
  in Just (unlines formatted, store)

total
processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store command = case parse command of
  Nothing => Just ("Invalid command\n", store)
  (Just (Add item)) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
  (Just (Search substring)) => searchStore store substring
  (Just (Get pos)) => getEntry pos store
  (Just Quit) => Nothing
  (Just Size) => Just ("Size: " ++ show (size store) ++ "\n", store)

main : IO ()
main = replWith (MkData _ []) "Command: " processInput
