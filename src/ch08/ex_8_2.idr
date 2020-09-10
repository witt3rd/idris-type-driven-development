import Data.Vect

%default total

myPlusCommutes : (n : Nat) -> (m : Nat) -> n + m = m + n
myPlusCommutes Z m =
  rewrite plusZeroRightNeutral m in Refl
myPlusCommutes (S k) m =
  rewrite myPlusCommutes k m in
    rewrite plusSuccRightSucc m k in Refl

reverseProof_nil : (acc : Vect n a) -> Vect (plus n 0) a
reverseProof_nil {n} acc = rewrite plusZeroRightNeutral n in acc

reverseProof_xs : Vect ((S n) + k) a -> Vect (n + (S k)) a
reverseProof_xs {n} {k} xs = rewrite sym (plusSuccRightSucc n k) in xs

myReverse : Vect n a -> Vect n a
myReverse xs = reverse' [] xs
  where reverse' : Vect n a -> Vect m a -> Vect (n+m) a
        reverse' acc [] = reverseProof_nil acc
        reverse' acc (x :: xs)
                        = reverseProof_xs (reverse' (x::acc) xs)
  
testMyReverse : Vect 4 Integer
testMyReverse = myReverse [1,2,3,4]