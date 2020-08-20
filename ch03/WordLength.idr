allLengths : List String -> List Nat
allLengths [] = []
allLengths (x :: xs) = length x :: allLengths xs

isEven : Nat -> Bool
isEven Z = True
isEven (S k) = not (isEven k)

