module Main (main) where

import Control.Exception (SomeException, catch)

-- Import a string from any file
import CS30.Exercises.TruthTable (debugOut)

{-

To print the value of a string variable on command line:

1. Import the variable from the file
2. Compile code and print the output : "stack exec debug --package cs30:debug"
3. Print the last compiled output : "stack exec debug"

-}

-- Print that string
main :: IO ()     
main = flip catch mkServerError $
       do putStrLn debugOut

-- Handle Error
mkServerError :: SomeException -> IO ()
mkServerError e = do putStrLn$ "Some error message:"
                     putStrLn (show e)
