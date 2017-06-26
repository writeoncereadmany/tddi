module Palindrome

export
palindrome : String -> Bool
palindrome str = let lowercase = toLower str in lowercase == (reverse lowercase)

lpalindrome : Nat -> String -> Bool
lpalindrome l str = length str > l && palindrome str

counts : String -> (Nat, Nat)
counts str = (length (words str), length str)

top_ten : List Nat -> List Nat
top_ten xs = take 10 (reverse $ sort xs)

over_length : Nat -> List String -> Nat
over_length x cs = length (filter (\c => length c > x) cs)
