module Main

import Data.Vect
-- import System.File

readToBlank : IO (List String)
readToBlank = do x <- getLine
                 if x == ""
                    then pure []
                    else do xs <- readToBlank
                            pure (x :: xs)

readAndSave : IO ()
readAndSave = do 
    input <- readToBlank
    putStrLn "Filename: "
    filename <- getLine
    Right f <- openFile filename WriteTruncate
      | Left err => putStrLn (show err)
    writeLines f input
    closeFile f
  where
    writeLines : File -> List String -> IO ()
    writeLines f [] = pure ()
    writeLines f (x :: xs) = do fPutStrLn f x
                                writeLines f xs

readVectFile : (filename : String) -> IO (n ** Vect n String)
readVectFile filename = do
    Right f <- openFile filename Read
      | Left err => do
        putStrLn (show err)
        pure (_ ** [])
    vect <- readVect f
    closeFile f
    pure vect
  where
    readVect : File -> IO (n ** Vect n String)
    readVect f = do
      isEOF <- fEOF f
      if isEOF
         then pure (_ ** [])
         else do
           Right x <- fGetLine f
             | Left err => do
               putStrLn (show err)
               pure (_ ** [])
           (_ ** xs) <- readVect f
           pure (_ ** (x :: xs))

