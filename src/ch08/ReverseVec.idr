import Data.Vect

myReverse : Vect n elem -> Vect n elem
myReverse [] = []
-- myReverse (x :: xs) = myReverse xs ++ [x]
myReverse (x :: xs) = prf (myReverse xs ++ [x])
  where
    prf : Vect (k + 1) elem -> Vect (S k) elem
    prf {k} result = rewrite plusCommutative 1 k in result
