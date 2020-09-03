public export
data EqNat : (num1 : Nat) -> (num2 : Nat) -> Type where
  Same : (num : Nat) -> EqNat num num

sameS : (k : Nat) -> (j : Nat) -> (eq : EqNat k j) -> EqNat (S k) (S j)
sameS k k (Same k) = Same (S k)

export
checkEqNat : (num1 : Nat) -> (num2 : Nat) -> Maybe (EqNat num1 num2)
checkEqNat Z      Z     = Just (Same Z)
checkEqNat Z      (S k) = Nothing
checkEqNat (S k)  Z     = Nothing
-- checkEqNat (S k)  (S j) = 
--   case checkEqNat k j of
--     Nothing => Nothing
--     Just eq => Just (sameS _ _ eq)
-- Ex 1: implemented as a case-split on eq
-- checkEqNat (S k)  (S j) = 
--   case checkEqNat k j of
--     Nothing => Nothing
--     Just (Same j) => Just (Same (S j))
-- Ex 2: implemented using do notation
checkEqNat (S k)  (S j) = 
  do Same j <- checkEqNat k j
     Just (Same (S j))