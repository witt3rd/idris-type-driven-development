data Vect : Nat -> Type -> Type where
  Nil  : Vect Z a
  (::) : (x : a) -> (xs : Vect k a) -> Vect (S k) a

Eq ty => Eq (Vect n ty) where
  (==) [] [] = True
  (==) (x :: xs) (y :: ys) = x == y && xs == ys

Foldable (Vect n) where
  foldr f acc [] = acc
  foldr f acc (x :: xs) = f x (foldr f acc xs)

testEq : Bool
testEq = the (Vect _ _) [1, 2, 3, 4] == [1, 2, 3, 4]

testFold : Integer
testFold = foldr (+) 0 (the (Vect _ _) [1, 2, 3, 4])