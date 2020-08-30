data Matter = Solid | Liquid | Gas

Eq Matter where
  (==) Solid    Solid   = True
  (==) Liquid   Liquid  = True
  (==) Gas      Gas     = True
  (==) _        _       = False
  
occurrences : Eq ty => (item : ty) -> (values : List ty) -> Nat
occurrences item []         = 0
occurrences item (x :: xs)  = case item == x of
                                   False => occurrences item xs
                                   True => S (occurrences item xs)

testMatter : Nat
testMatter = occurrences Gas [Solid, Gas, Liquid, Gas, Solid, Gas, Gas]

data Tree elem = Empty
               | Node (Tree elem) elem (Tree elem)

Eq elem => Eq (Tree elem) where
  (==) Empty        Empty           = True
  (==) (Node x y z) (Node x' y' z') = (x == x') && (y == y') && (z == z') 
  (==) _            _               = False

testTree : Bool
testTree = 
  let x = Node Empty 1 Empty
      y = Node Empty 1 Empty in
  x == y