data SplitList : List a -> Type where 
     SplitNil : SplitList []
     SplitOne : SplitList [x]
     SplitPair : (lefts : List a) -> (rights : List a) -> SplitList (lefts ++ rights)

splitList : (input : List a) -> SplitList input

mergeSort : Ord a => List a -> List a
mergeSort [] = []
mergeSort [x] = [x]
mergeSort (lefts ++ rights) = merge (mergeSort lefts) (mergeSort rights)
