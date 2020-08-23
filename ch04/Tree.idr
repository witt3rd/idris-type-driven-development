module Tree

%default total

public export
data Tree a = Empty
            | Node (Tree a) a (Tree a)

%name Tree tree, tree1

export
insert : Ord a => a -> Tree a -> Tree a
insert x Empty = Node Empty x Empty
insert x orig@(Node left val right) = 
  case compare x val of
       LT => Node (insert x left) val right
       EQ => orig
       GT => Node left val (insert x right)
                               

