TupleVect : (n : Nat) -> (ty: Type) -> Type
TupleVect Z ty = ()
TupleVect (S k) ty = (ty, TupleVect k ty)
