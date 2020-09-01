data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Functor Tree where
  map func Empty                = Empty
  map func (Node left e right)  = Node (map func left) (func e) (map func right)

Foldable Tree where
  foldr func acc Empty                  = acc
  foldr func acc (Node left elem right) =
    let leftfold  = foldr func acc left
        rightfold = foldr func leftfold right in
          func elem rightfold

testFold : Nat
testFold = 
  let ll  = Node Empty  1 Empty
      lr  = Node Empty  3 Empty
      l   = Node ll     2 lr
      rl  = Node Empty  5 Empty
      rr  = Node Empty  7 Empty
      r   = Node rl     6 rr
      t   = Node l      4 r     in
        foldr (\elem, acc => elem + acc) 0 t