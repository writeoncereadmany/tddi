data BSTree : Type -> Type where
  Empty : Ord elem => BSTree elem
  Node  : Ord elem => (left : BSTree elem) -> (val : elem) -> (right : BSTree elem) -> BSTree elem

insert : elem -> BSTree elem -> BSTree elem
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) =
  case compare x val of
    LT => Node (insert x left) val right
    EQ => orig
    GT => Node left val (insert x right)

listToTree : Ord elem => List elem -> BSTree elem
listToTree = foldl (flip insert) Empty



foldl_tree : (func : acc -> elem -> acc) -> (init : acc) -> (input : BSTree elem) -> acc
foldl_tree func init Empty = init
foldl_tree func init (Node left val right) = foldl_tree func to_centre right where
    to_left : acc
    to_left = foldl_tree func init left
    to_centre : acc
    to_centre = func to_left val


foldr_tree : (func : elem -> acc -> acc) -> (init : acc) -> (input : BSTree elem) -> acc
foldr_tree func init Empty = init
foldr_tree func init (Node left val right) = foldr_tree func to_centre left where
    to_right : acc
    to_right = foldr_tree func init right
    to_centre : acc
    to_centre = func val to_right

implementation Foldable BSTree where
  foldl = foldl_tree
  foldr = foldr_tree

treeToList : Ord elem => BSTree elem -> List elem
treeToList = toList
