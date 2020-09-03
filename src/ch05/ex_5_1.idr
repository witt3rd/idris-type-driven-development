module Main

-- import Data.Strings

longer : String -> String -> Nat
longer x y = max (length x) (length y)

printLonger : IO ()
printLonger = do
  putStrLn "First string: "
  x <- getLine
  putStrLn "Second string: "
  y <- getLine
  putStrLn (show (longer x y))

printLonger2 : IO ()
printLonger2 = do
  putStrLn "First string: " >>= \_ => 
    getLine >>= \x =>
      putStrLn "Second string: " >>= \_ =>
        getLine >>= \y =>
          putStrLn (show (longer x y))


main : IO ()
main = do printLonger2
