import Data.Vect

Matrix : Nat -> Nat -> Type -> Type
Matrix m n elem = Vect m (Vect n elem)

addMatrix : Num num => Matrix m n num -> Matrix m n num -> Matrix m n num
addMatrix xs ys = zipWith (zipWith (+)) xs ys

transposeMatrix : Matrix m n elem -> Matrix n m elem
transposeMatrix [] = replicate _ []
transposeMatrix (x :: xs) = zipWith (::) x (transposeMatrix xs)

multRow : Num num => Matrix p m num -> Vect m num -> Vect p num
multRow ys x = map (sum . zipWith (*) x) ys

multMatrix : Num num => Matrix n m num -> Matrix m p num -> Matrix n p num
multMatrix xs ys = map (multRow $ transposeMatrix ys) xs
