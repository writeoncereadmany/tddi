module Main

import Data.Vect
import Data.String

infixr 5 .+.

data Schema = SString
            | SInt
            | (.+.) Schema Schema

SchemaType : Schema -> Type
SchemaType SString = String
SchemaType SInt = Int
SchemaType (x .+. y) = (SchemaType x, SchemaType y)

record DataStore where
  constructor MkData
  schema : Schema
  size : Nat
  items : Vect size (SchemaType schema)

total
addToStore : (store : DataStore) -> SchemaType (schema store) -> DataStore
addToStore (MkData schema size items) newItem = MkData schema _ (addToData items)
  where
    addToData : Vect old (SchemaType schema) -> Vect (S old) (SchemaType schema)
    addToData [] = [newItem]
    addToData (x :: xs) = x :: addToData xs

data Command : Schema -> Type where
  SetSchema : (newSchema : Schema) -> Command schema
  Add : SchemaType schema -> Command schema
  Get : Integer -> Command schema
  Size : Command schema
  Quit : Command schema

total
joinString : (separator : String) -> (strings : List String) -> String
joinString separator [] = ""
joinString separator [x] = x
joinString separator (x :: y :: xs) = x ++ separator ++ joinString separator (y :: xs)

parsePrefix : (schema : Schema) -> String -> Maybe (SchemaType schema, String)
parsePrefix SString input = getQuoted (unpack input) where
  getQuoted : List Char -> Maybe (String, String)
  getQuoted ('"' :: chars) = case span (/= '"') chars of
    (quoted, '"' :: rest) => Just (pack quoted, ltrim $ pack rest)
    _ => Nothing
  getQuoted _ = Nothing
parsePrefix SInt input = case span isDigit input of
                           ("", rest) => Nothing
                           (num, rest) => Just (cast num, ltrim rest)
parsePrefix (schema1 .+. schema2) input = do
                      (result1, rest1) <- parsePrefix schema1 input
                      (result2, rest2) <- parsePrefix schema2 rest1
                      Just ((result1, result2), rest2)

parseBySchema : (schema : Schema) -> (arg : String) -> Maybe (SchemaType schema)
parseBySchema schema arg = do (res, "") <- parsePrefix schema arg | _ => Nothing
                              Just res

mutual
  parseSchema : List String -> Maybe Schema
  parseSchema ("String" :: rest) = parseRest SString rest
  parseSchema ("Int" :: rest) = parseRest SInt rest
  parseSchema _ = Nothing

  parseRest : Schema -> List String -> Maybe Schema
  parseRest schema [] = Just schema
  parseRest schema rest = map (schema .+.) (parseSchema rest)

total
parseCommand : (schema : Schema) -> (command : String) -> (arg : String) -> Maybe (Command schema)
parseCommand schema "add" arg = map Add (parseBySchema schema arg)
parseCommand _ "setSchema" arg = map SetSchema (parseSchema (words arg))
-- parseCommand "search" arg = Just (Search arg)
parseCommand _ "get" arg = map Get (parseInteger arg)
parseCommand _ "size" _  = Just Size
parseCommand _ "quit" _  = Just Quit
parseCommand _ _ _ = Nothing

total
parse : (schema : Schema) -> (input : String) -> Maybe (Command schema)
parse schema input = case span (/= ' ') input of
  (command, arg) => parseCommand schema command (ltrim arg)

total
display : SchemaType schema -> String
display { schema = SString } item = item
display { schema = SInt } item = show item
display { schema = (y .+. z) } (a, b) = display a ++ ", " ++ display b

total
getEntry : (pos : Integer) -> (store : DataStore) -> Maybe (String, DataStore)
getEntry pos store =
  let storedItems = items store
  in case integerToFin pos (size store) of
    Nothing => Just ("Out of range\n", store)
    (Just i) => let item = display (index i storedItems)
                in Just (item ++ "\n", store)

total
withIndices : Vect n elem -> List (Nat, elem)
withIndices { n } xs = zip [1..n] (toList xs)

-- total
-- searchStore : (store : DataStore) -> (substring : String) -> Maybe (String, DataStore)
-- searchStore store substring =
--   let indexed = withIndices (items store)
--       matching = filter ((isInfixOf substring) . snd) indexed
--       formatted = map (\(i, val) => show i ++ ": " ++ val) matching
--   in Just (unlines formatted, store)

total
processInput : (store: DataStore) -> String -> Maybe (String, DataStore)
processInput store command = case parse (schema store) command of
  Nothing => Just ("Invalid command\n", store)
  (Just (Add item)) => Just ("ID " ++ show (size store) ++ "\n", addToStore store item)
  -- (Just (Search substring)) => searchStore store substring
  (Just (Get pos)) => getEntry pos store
  (Just Quit) => Nothing
  (Just Size) => Just ("Size: " ++ show (size store) ++ "\n", store)
  (Just (SetSchema schema)) => if size store == 0
    then Just ("Schema applied\n", MkData schema 0 [])
    else Just ("Cannot change schema once store contains items\n", store)


main : IO ()
main = replWith (MkData SString _ []) "Command: " processInput
