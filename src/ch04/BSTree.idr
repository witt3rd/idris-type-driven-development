module BSTree

%default total

data BSTree : Type -> Type where
  Empty : Ord a => BSTree a
  Node : Ord a => (left : BSTree a) -> (val : a) -> (right : BSTree a) -> BSTree a

insert : a -> BSTree a -> BSTree a
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) =
  case compare x val of
       LT : Node (insert x left) val right
       EQ : orig
       GT : Node left val (insert x right)

