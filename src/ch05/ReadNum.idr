module Main

import Data.Strings

%default total

readNumber : IO (Maybe Nat)
readNumber = do
  input <- getLine
  if all isDigit (unpack input)
     then pure (Just (stringToNatOrZ input))
     else pure Nothing

readNumbers : IO (Maybe (Nat, Nat))
readNumbers =
  do num1 <- readNumber
     case num1 of
          Nothing => pure Nothing
          Just num1_ok =>
                         do num2 <- readNumber
                            case num2 of
                                 Nothing => pure Nothing
                                 Just num2_ok => pure (Just (num1_ok, num2_ok))

readNumbers2 : IO (Maybe (Nat, Nat))
readNumbers2 = do Just num1_ok <- readNumber | Nothing => pure Nothing
                  Just num2_ok <- readNumber | Nothing => pure Nothing
                  pure (Just (num1_ok, num2_ok))

readPair : IO (String, String)
readPair = do str1 <- getLine
              str2 <- getLine
              pure (str1, str2)

usePair : IO ()
usePair = do pair <- readPair
             case pair of
                  (str1, str2) => putStrLn ("You entered " ++ str1 ++ " and " ++ str2)

usePair2 : IO ()
usePair2 = do (str1, str2) <- readPair
              putStrLn ("You entered " ++ str1 ++ " and " ++ str2)


