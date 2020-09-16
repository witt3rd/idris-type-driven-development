import Data.Vect

%access total

diagHelper : Vect n (Vect m Int) ->
             (i : Integer) ->
             (lrSum : Int) ->
             (rlSum : Int) ->
             Maybe Int
diagHelper [] _ lrSum rlSum = Just (abs (lrSum - rlSum))
diagHelper {m} (row :: rows) i lrSum rlSum = do
  lrIdx <- integerToFin i m
  rlIdx <- integerToFin (((toIntegerNat m) - 1) - i) m
  diagHelper rows (i + 1) (lrSum + (index lrIdx row)) (rlSum + (index rlIdx row))

diagDiff : Vect n (Vect n Int) -> Maybe Int
diagDiff xs = diagHelper xs 0 0 0

testSquare1 : Vect 3 (Vect 3 Int)
testSquare1 = [
  [1, 2, 3],
  [4, 5, 6],
  [7, 8, 9]
]

testSquare2 : Vect 3 (Vect 3 Int)
testSquare2 = [
  [11, 2, 4],
  [4, 5, 6],
  [10, 8, -12]
]
