printLength : IO ()
printLength = getLine >>= \result => putStrLn (show $ length result)

printLonger : IO ()
printLonger = do
  first <- getLine
  second <- getLine
  let maxLength = max (length first) (length second)
  putStrLn (show maxLength)
