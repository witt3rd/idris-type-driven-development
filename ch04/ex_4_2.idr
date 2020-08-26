import Data.Vect

%default total

vectTake : (n : Nat) -> Vect (n + m) a -> Vect n a
vectTake 0 _ = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

sumEntries : Num a => {n : _} -> (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries pos [] [] = Nothing
sumEntries {n} pos xs ys = case integerToFin pos n of
                                Nothing => Nothing
                                (Just idx) => let x = index idx xs
                                                  y = index idx ys in
                                                  Just $ x + y

