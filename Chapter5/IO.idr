readToBlank : IO (List String)
readToBlank = do next <- getLine
                 if next == ""
                    then pure []
                    else do rest <- readToBlank
                         pure (next::rest)
