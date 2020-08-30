module Main

StringOrInt : Bool -> Type
StringOrInt True = Int
StringOrInt False = String

getStringOrInt : (x : Bool) -> StringOrInt x
getStringOrInt True = 94
getStringOrInt False = "Ninety four"

valToString : (x : Bool) -> StringOrInt x -> String
valToString True val = cast val
valToString False val = val

main : IO ()
main = putStrLn (valToString False "42")
