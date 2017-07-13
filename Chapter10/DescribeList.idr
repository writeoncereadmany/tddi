data ListLast : List a -> Type where
  Empty : ListLast []
  NonEmpty : (xs : List a) -> (x : a) -> ListLast (xs ++ [x])

total
listLast : (xs : List a) -> ListLast xs
listLast [] = Empty
listLast (x :: xs) = case listLast xs of
                      Empty => NonEmpty [] x
                      NonEmpty ys y => NonEmpty (x :: ys) y

describeHelper : (input : List Int) -> (form : ListLast input) -> String
describeHelper [] Empty = "Empty"
describeHelper (xs ++ [x]) (NonEmpty xs x) = "Non-empty, initial portion " ++ show xs

describeListEnd : List Int -> String
describeListEnd xs with (listLast xs)
  describeListEnd [] | Empty = "Empty"
  describeListEnd (ys ++ [x]) | (NonEmpty ys x) = "Non empty, starting with " ++ show ys

myReverse : List a -> List a
myReverse xs with (listLast xs)
  myReverse [] | Empty = []
  myReverse (ys ++ [x]) | (NonEmpty ys x) = x :: myReverse ys

data SnocList : List a -> Type where
  SnocEmpty : SnocList []
  Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])

snocListHelp : (snoc : SnocList input) -> (rest : List a) -> SnocList (input ++ rest)
snocListHelp { input } snoc [] = rewrite appendNilRightNeutral input in snoc
snocListHelp { input } snoc (x :: xs) = rewrite appendAssociative input [x] xs in
                                                snocListHelp (Snoc snoc {x}) xs

snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelp SnocEmpty xs

total
myReverseHelper : (input : List a) -> SnocList input -> List a
myReverseHelper [] SnocEmpty = []
myReverseHelper (xs ++ [x]) (Snoc rec) = x :: myReverseHelper xs rec

total
myReverse' : List a -> List a
myReverse' xs = myReverseHelper xs (snocList xs)

myReverse'' : List a -> List a
myReverse'' input with (snocList input)
  myReverse'' [] | SnocEmpty = []
  myReverse'' (xs ++ [x]) | (Snoc rec) = x :: myReverse xs | rec

isSuffix : Eq a => List a -> List a -> Bool
isSuffix suffix list with (snocList suffix)
  isSuffix [] list | SnocEmpty = True
  isSuffix (xs ++ [x]) list | (Snoc xsrec) with (snocList list)
    isSuffix (xs ++ [x]) [] | (Snoc xsrec) | SnocEmpty = False
    isSuffix (xs ++ [x]) (ys ++ [y]) | (Snoc xsrec) | (Snoc ysrec) =
      if x == y
      then isSuffix xs ys | xsrec | ysrec
      else False
