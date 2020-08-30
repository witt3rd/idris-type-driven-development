module Main

import Data.Strings
import Data.List
import System.REPL

average : (str : String) -> Double
average str = let numWords = wordCount str
                  totalLength = sum (allLengths (words str)) in
                  cast totalLength / cast numWords
  where
    wordCount : String -> Nat
    wordCount str = length (words str)

    allLengths : List String -> List Nat
    allLengths strs = map length strs

showAverage : String -> String
showAverage str = "The average word length is: " ++ 
                  show (average str) ++ "\n"

counts : String -> (Nat, Nat)
counts x = (length (words x), length x) 

top_ten : Ord a => List a -> List a
top_ten xs = take 10 (sort xs)

over_length : Nat -> List String -> Nat
over_length k xs = length $ filter (\x => length x > k) xs

palindrome : Nat -> String -> Bool
palindrome x str = if length str < x then False else
    let lstr = toLower str in
      lstr == (reverse lstr) 

showPalindrome : String -> String
showPalindrome str = show (palindrome 5 str) ++ "\n"

main : IO ()
main = repl "Enter a string: " showPalindrome
