module Main

import System
import ch05.ReadNum

guess : (target : Nat) -> (guesses : Nat) -> IO ()
guess target guesses =
  do putStr ((show guesses) ++ ") Guess a number: ")
     Just g <- readNumber
        | Nothing => do putStrLn "Invalid input"
                        guess target guesses
     case compare g target of
          LT => do putStrLn ("Too low")
                   guess target (S guesses)
          GT => do putStrLn ("Too high")
                   guess target (S guesses)
          EQ => do putStrLn ("Correct!")
                   pure ()

main : IO ()
main = do t <- time
          let target = integerToNat (t `mod` 100)
          guess target Z

myRepl : (prompt : String) -> (onInput : String -> String) -> IO ()
myRepl prompt onInput = 
  do putStr prompt
     input <- getLine
     let output = onInput input
     putStrLn output
     myRepl prompt onInput

myReplWith : (state : s) -> (prompt : String) -> (onInput : s -> String -> Maybe (String, s)) -> IO ()
myReplWith state prompt onInput = 
  do putStr prompt
     input <- getLine
     case onInput state input of
          Nothing => pure ()
          Just (output, state') => do putStrLn output
                                      myReplWith state' prompt onInput

testMyRepl : IO ()
testMyRepl = myRepl "> " \x => show (length x)
  
testMyReplWith : IO ()
testMyReplWith = myReplWith "" "> " \x, y => if y == "-q" then Nothing else Just ("Previous: " ++ x, y)
