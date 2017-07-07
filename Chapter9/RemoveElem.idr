import Data.Vect

oneInVector : Elem 1 [1,2,3]

maryInVector : Elem "mary" ["peter", "paul", "mary"]
maryInVector = There (There Here)

fourNotInVector : Elem 4 [1,2,3] -> Void
peteNotInVector: Elem "Pete" ["John", "Paul", "George", "Ringo"] -> Void

removeElem : (value : a) -> (xs : Vect (S n) a) -> (prf : Elem value xs) -> Vect n a
removeElem value (value :: ys) Here = ys
removeElem { n  =  Z } value (y :: []) (There later) = absurd later
removeElem { n  =  (S k) } value (y :: ys) (There later) = y :: removeElem value ys later


removeElem' : (value : a) -> (xs : Vect (S n) a) -> {auto prf : Elem value xs} -> Vect n a
removeElem' value xs { prf } = removeElem value xs prf
