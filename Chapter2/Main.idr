module Main

import Palindrome

show_palindrome : String -> String
show_palindrome str = show (palindrome str)

main : IO ()
main = repl "> " show_palindrome
