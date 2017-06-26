data Expr = Literal Integer
          | Sum Expr Expr
          | Diff Expr Expr
          | Product Expr Expr

evaluate : Expr -> Integer
evaluate (Literal x) = x
evaluate (Sum x y) = evaluate x + evaluate y
evaluate (Diff x y) = evaluate x - evaluate y
evaluate (Product x y) = evaluate x * evaluate y

maxMaybe : Ord a => Maybe a -> Maybe a -> Maybe a
maxMaybe Nothing Nothing = Nothing
maxMaybe Nothing only@(Just x) = only
maxMaybe only@(Just x) Nothing = only
maxMaybe (Just x) (Just y) = Just (max x y)
