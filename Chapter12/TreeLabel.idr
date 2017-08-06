import Control.Monad.State

data Tree a = Empty
            | Node (Tree a) (a) (Tree a)

testTree : Tree String
testTree = Node ( Node ( Node Empty "Jim" Empty )
                       "Fred"
                       ( Node Empty "Sheila" Empty )
                ) "Alice"
                ( Node Empty "Bob" (Node Empty "Eve" Empty ))

flatten : Tree a -> List a
flatten Empty = []
flatten (Node left val right) = flatten left ++ val :: flatten right

treeLabelWith : Stream labelType -> Tree a -> (Stream labelType, Tree (labelType, a))
treeLabelWith labels Empty = (labels, Empty)
treeLabelWith labels (Node left val right)
  = let (lblThis :: rest_labels, left_labelled) = treeLabelWith labels left
        (leftovers, right_labelled) = treeLabelWith rest_labels right
    in (leftovers, Node left_labelled (lblThis, val) right_labelled)

treeLabel : Tree a -> Tree (Integer, a)
treeLabel = snd . (treeLabelWith [1..])

treeLabelWith2 : Tree a -> State (Stream labelType) (Tree (labelType, a))
treeLabelWith2 Empty = pure Empty
treeLabelWith2 (Node left val right) = do left_labelled <- treeLabelWith2 left
                                          (this :: rest) <- get
                                          put rest
                                          right_labelled <- treeLabelWith2 right
                                          pure (Node left_labelled (this, val) right_labelled)


treeLabelWith3 : Tree a -> State Integer (Tree (Integer, a))
treeLabelWith3 Empty = pure Empty
treeLabelWith3 (Node left val right) = do left_labelled <- treeLabelWith3 left
                                          x <- get
                                          put (x + 1)
                                          right_labelled <- treeLabelWith3 right
                                          pure (Node left_labelled (x, val) right_labelled)

update : (stateType -> stateType) -> State stateType ()
update f = do current <- get
              put (f current)

increase : Nat -> State Nat ()
increase inc = update (+ inc)

incLeft : Nat -> State (Nat, Nat) ()
incLeft inc = update leftPlus where
  leftPlus : (Nat, Nat) -> (Nat, Nat)
  leftPlus (x, y) = (x + inc, y)


incRight : Nat -> State (Nat, Nat) ()
incRight inc = update rightPlus where
  rightPlus : (Nat, Nat) -> (Nat, Nat)
  rightPlus (x, y) = (x, y + inc)


countEmpties : Tree a -> State Nat ()
countEmpties Empty = increase 1
countEmpties (Node x y z) = do left <- countEmpties x
                               right <- countEmpties z
                               increase 0

counts : Tree a -> State (Nat, Nat) ()
counts Empty = incLeft 1
counts (Node left val right) = do l <- counts left
                                  r <- counts right
                                  incRight 1
