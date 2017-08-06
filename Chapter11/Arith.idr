
quiz : Stream Int -> (score : Nat) -> IO ()
quiz (x :: y :: rest) score =
  do putStrLn ("Score so far: " ++ show score)
     putStrLn (show x ++ " * " ++ show y ++ "? ")
     answer <- getLine
     if cast answer == x * y
       then do putStrLn "Correct!"
               quiz rest (score + 1)
       else do putStrLn "Wrong!"
               quiz rest score

randoms : Int -> Stream Int
randoms seed = let seed' = 1664535 * seed + 1013904223 in
                  (seed' `shiftR` 2) :: randoms seed'

rightZero : (k : Nat) -> k = plus k 0
rightZero Z = Refl
rightZero (S k) = rewrite rightZero k in Refl

liftS : (n : Nat) -> (m : Nat) -> S (plus n m) = plus n (S m)
liftS Z m = Refl
liftS (S k) m = rewrite liftS k m in Refl

pc : (n : Nat) -> (m : Nat) -> n + m = m + n
pc Z Z = Refl
pc Z (S k) = rewrite rightZero k in Refl
pc (S k) m = rewrite sym (liftS m k) in rewrite pc k m in Refl

pc2 : (i : Integer) -> (j : Integer) -> i + j = j + i
pc2 i j = ?pc2_rhs
