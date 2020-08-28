import Data.Strings

StringOrInt : Bool -> Type
StringOrInt True = Int 
StringOrInt False = String

getStringOrInt : (isInt : Bool) -> StringOrInt isInt
getStringOrInt True = 94
getStringOrInt False = "\t\t\tninety four        "

valToString : (isInt : Bool) -> StringOrInt isInt -> String
valToString True x = cast x
valToString False x = trim x

valToString2 : (isInt : Bool) -> (case isInt of
                                       True => Int
                                       False => String) -> String
valToString2 True x = cast x
valToString2 False x = trim x

test : Bool -> String
test x = valToString x (getStringOrInt x)

test2 : Bool -> String
test2 True = valToString2 True 94
test2 False = valToString2 False "\t\t\tninety four        "

main : IO ()
main = do
  putStrLn $ "False = " ++ (test False)
  putStrLn $ "True = " ++ (test True)
