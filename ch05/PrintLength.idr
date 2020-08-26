module PrintLength

printLength : IO ()
printLength = putStr "Input string: " >>= \_ => getLine >>= \input => putStrLn $ show (length input)

printTwoThings : IO ()
printTwoThings = do putStrLn "Hello"
                    putStrLn "World"

printInput : IO ()
printInput = do x <- getLine
                putStrLn x

printLength2 : IO ()
printLength2 = do putStr "Input string: "
                  input <- getLine
                  let len = length input
                  putStrLn (show len)
