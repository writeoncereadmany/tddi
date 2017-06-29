module Main

myrepl : String -> (String -> String) -> IO ()
myrepl prompt function = do putStrLn prompt
                            input <- getLine
                            putStrLn (function input)
                            myrepl prompt function

myreplwith : a -> String -> (a -> String -> Maybe (String, a)) -> IO ()
myreplwith context prompt function = do putStrLn prompt
                                        input <- getLine
                                        case function context input of
                                          Nothing => pure ()
                                          (Just (output, newContext)) => do
                                            putStrLn output
                                            myreplwith newContext prompt function
