import Data.Vect

readVectLen : (len : Nat) -> IO (Vect len String)
readVectLen Z = pure []
readVectLen (S k) = do x <- getLine
                       xs <- readVectLen k
                       pure (x :: xs)

readVect : IO (n ** Vect n String)
readVect = do x <- getLine
              if x == ""
                then pure (_ ** [])
                else do (_ ** xs) <- readVect
                        pure (_ ** x :: xs)

zipInputs : IO ()
zipInputs = do putStrLn "Enter first vector (blank to end):"
               (xLen ** xs) <- readVect
               putStrLn "Enter second vector (blank to end):"
               (yLen ** ys) <- readVect
               case exactLength xLen ys of
                 Nothing => putStrLn "Vector lengths didn't match: can't zip"
                 (Just newYs) => printLn (zip xs newYs)

main : IO ()
main = readVect >>= (putStrLn . show)
