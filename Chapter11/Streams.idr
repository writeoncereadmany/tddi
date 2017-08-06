

labelWith : (labels: Stream labelType) -> (values: List a) -> List (labelType, a)
labelWith _ [] = []
labelWith (label :: labels) (value :: values) = (label, value) :: labelWith labels values

label : List a -> List (Integer, a)
label xs = labelWith [1..] xs
