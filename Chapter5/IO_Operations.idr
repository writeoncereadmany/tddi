module Main

import Data.Vect

readToBlank : IO (List String)
readToBlank = do input <- getLine
                 if input == ""
                 then pure []
                 else do rest <- readToBlank
                         pure (input :: rest)

readAndSave : IO ()
readAndSave = do printLn "Enter a list of items, then blank to finish"
                 list <- readToBlank
                 printLn "Enter a file to save to"
                 fileName <- getLine
                 Right () <- writeFile fileName (unlines list)
                   | Left error => printLn ("File error: " ++ show error)
                 printLn "Successfully saved file"

readFileToVect : (file : File) -> IO (n : Nat ** Vect n String)
readFileToVect file = do False <- fEOF file
                           | True => pure (_ ** [])
                         Right line <- fGetLine file
                           | Left err => pure (_ ** [])
                         (_ ** rest) <- readFileToVect file
                         pure (_ ** line::rest)


readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do Right file <- openFile filename Read
                             | Left fileError => pure (_ ** [])
                           result <- readFileToVect file
                           closeFile file
                           result
