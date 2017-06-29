AdderType : (numArgs : Nat) -> Type
AdderType Z = Int
AdderType (S k) = (next : Int) -> AdderType k

adder : (numArgs : Nat) -> (acc : Int) -> AdderType numArgs
adder Z acc = acc
adder (S k) acc = \next => adder k (next + acc)
