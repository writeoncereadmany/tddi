import Data.Vect

list_length : List a -> Nat
list_length [] = 0
list_length (x :: xs) = 1 + list_length xs

reverse_list : List a -> List a
reverse_list [] = ?reverse_list_rhs_1
reverse_list (x :: xs) = reverse_list xs ++ [x]

map_list : (a -> b) -> List a -> List b
map_list f [] = []
map_list f (x :: xs) = f x :: map_list f xs

map_vect : (a -> b) -> Vect k a -> Vect k b
map_vect f [] = []
map_vect f (x :: xs) = f x :: map_vect f xs
