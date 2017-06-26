import Data.Fin
import Data.Vect

data MyVect : Nat -> Type -> Type where
  Nil : MyVect Z a
  (::) : (x : a) -> (xs : MyVect k a) -> MyVect (S k) a

append : MyVect n elem -> MyVect m elem -> MyVect (n + m) elem
append [] ys = ys
append (x :: xs) ys = x :: append xs ys

zip : MyVect n a -> MyVect n b -> MyVect n (a, b)
zip [] [] = []
zip (x :: xs) (y :: ys) = (x, y) :: zip xs ys

tryIndex : Integer -> Vect n a -> Maybe a
tryIndex { n } i xs = map (flip index xs) (integerToFin i n)

vectTake : (n : Nat) -> MyVect (n + m) elem -> MyVect n elem
vectTake Z _ = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

sumEntries : Num a => (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries { n } pos xs ys =
    let fpos = integerToFin pos n
        sums = zipWith (+) xs ys
    in map (flip index sums) fpos
