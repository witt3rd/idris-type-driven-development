total
twoPlusTwoNotFive : 2 + 2 = 5 -> Void
twoPlusTwoNotFive Refl impossible

loop : Void
loop = loop

valueNotSucc : (x : Nat) -> x = S x -> Void
valueNotSucc _ Refl impossible

void : Void -> a
