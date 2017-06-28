module Main

main : IO ()
main = do
  putStrLn "What's your name? :) "
  name <- getLine
  putStrLn ("Hello " ++ name ++ "!")
