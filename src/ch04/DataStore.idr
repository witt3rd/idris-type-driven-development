module Main

import Data.Strings
import Data.Vect
import System.REPL

data DataStore : Type where
  MkData : (size : Nat) ->
           (items : Vect size String) ->
           DataStore

size : DataStore -> Nat
size (MkData size' _) = size'

items : (store: DataStore) -> Vect (size store) String
items (MkData _ items') = items'

addToStore : String -> DataStore -> DataStore
addToStore newItem (MkData size items) = MkData _ (addToData items) where
  addToData : Vect old String -> Vect (S old) String
  addToData [] = [newItem]
  addToData (x :: xs) = x :: addToData xs

getEntry : Integer -> DataStore -> Maybe (String, DataStore)
getEntry idx store@(MkData size items) = case integerToFin idx size of
                                              Nothing => Just ("Out of range\n", store)
                                              Just id => Just (index id items ++ "\n", store)

filterItems : Nat -> String -> Vect n String -> (Integer, String)
filterItems _ _ [] = (0, "")
filterItems idx str (x :: xs) = case isInfixOf str x of
                                     False => filterItems (S idx) str xs
                                     True => let (i, s) = filterItems (S idx) str xs in
                                                 (i + 1, show idx ++ ": " ++ x ++ "\n" ++ s)

search : String -> DataStore -> Maybe (String, DataStore)
search str store@(MkData size items) = 
  case filterItems Z str items of
       (0, _) => Just ("Not found\n", store)
       (_, results) => Just (results, store)

data Command = Add String
             | Get Integer
             | Search String
             | Size
             | Quit

parseCommand : (cmd : String) -> (args : String) -> Maybe Command
parseCommand "add" str = Just (Add str)
parseCommand "get" val = case all isDigit (unpack val) of
                              False => Nothing
                              True => Just (Get (cast val))
parseCommand "search" str = Just (Search str)
parseCommand "size" "" = Just Size
parseCommand "quit" "" = Just Quit
parseCommand _ _ = Nothing

parse : (input : String) -> Maybe Command
parse input = case span (/= ' ') input of
                   (cmd, args) => parseCommand cmd (ltrim args)

processInput : DataStore -> String -> Maybe (String, DataStore)
processInput store inp = case parse inp of
                              Nothing => Just ("Invalid command\n", store)
                              Just (Add item) => Just ("ID " ++ show (size store) ++ "\n", addToStore item store)
                              Just (Get idx) => getEntry idx store
                              Just (Search str) => search str store
                              Just Size => Just ("Size " ++ show (size store) ++ "\n", store)
                              Just Quit => Nothing


main : IO ()
main = replWith (MkData _ []) "Command: " processInput
