totalLength : List String -> Nat
totalLength xs = foldr (\str, len => length str + len) 0 xs
