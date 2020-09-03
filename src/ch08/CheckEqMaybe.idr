export        
checkEqNat' : (num1 : Nat) -> (num2 : Nat) -> Maybe (num1 = num2)
checkEqNat' Z      Z     = Just Refl
checkEqNat' Z      (S k) = Nothing
checkEqNat' (S k)  Z     = Nothing
checkEqNat' (S k)  (S j) = 
  do prf <- checkEqNat' k j
     Just (cong prf)