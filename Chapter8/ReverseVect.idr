import Data.Vect

reverseProof : Vect (k + 1) elem -> Vect (S k) elem
reverseProof {k} result = rewrite plusCommutative 1 k in result

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
myReverse (x :: xs) = reverseProof (myReverse xs ++ [x])

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes n Z = rewrite plusZeroRightNeutral n in Refl
myPlusCommutes n (S k) = rewrite sym (plusSuccRightSucc n k) in
                           rewrite myPlusCommutes n k in Refl

myPlusCommutes' : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes' Z m = rewrite plusZeroRightNeutral m in Refl
myPlusCommutes' (S k) m = rewrite myPlusCommutes' k m in rewrite plusSuccRightSucc m k in Refl
