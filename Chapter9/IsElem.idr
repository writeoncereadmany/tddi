import Data.Vect

data Elem' : a -> Vect k a -> Type where
  Here : Elem' x (x :: xs)
  There : (later : Elem' x xs) -> Elem' x (y :: xs)

nothingInNil : Elem' value [] -> Void
nothingInNil Here impossible
nothingInNil (There _) impossible

notInTail : (notHere : (value = x) -> Void) -> (notThere : Elem' value xs -> Void) -> Elem' value (x :: xs) -> Void
notInTail notHere notThere Here = notHere Refl
notInTail notHere notThere (There later) = notThere later

isElem' : DecEq a => (value : a) -> (xs : Vect n a) -> Dec (Elem' value xs)
isElem' value [] = No nothingInNil
isElem' value (x :: xs) = case decEq value x of
                               Yes Refl => Yes Here
                               No notHere => case isElem' value xs of
                                                  Yes prf => Yes (There prf)
                                                  No notThere => No (notInTail notHere notThere)

data ElemList : a -> List a -> Type where
  Heady : ElemList x (x :: xs)
  Taily : (later : ElemList x xs) -> ElemList x (y :: xs)

data Last : List a -> a -> Type where
  LastOne : Last [x] x
  LastCons : (prf: Last xs value) -> Last (x::xs) value

last123 : Last [1,2,3] 3
last123 = LastCons (LastCons LastOne)


notLastOfEmpty : Last [] value -> Void
notLastOfEmpty LastOne impossible
notLastOfEmpty (LastCons _) impossible

notLastOfOne : (contra : (x = value) -> Void) -> Last [x] value -> Void
notLastOfOne contra LastOne = contra Refl
notLastOfOne contra (LastCons prf) = notLastOfEmpty prf

notLastOfTail : (contra : Last (y :: rest) value -> Void) -> Last (x :: (y :: rest)) value -> Void
notLastOfTail contra (LastCons prf) = contra prf

isLast : DecEq a => (xs : List a) -> (value : a) -> Dec (Last xs value)
isLast [] value = No notLastOfEmpty
isLast [x] value = case decEq x value of
                        (Yes Refl) => Yes LastOne
                        (No contra) => No (notLastOfOne contra)
isLast (x::y::rest) value = case isLast (y::rest) value of
                                 (Yes prf) => Yes (LastCons prf)
                                 (No contra) => No (notLastOfTail contra)
