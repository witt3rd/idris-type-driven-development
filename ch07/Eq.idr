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