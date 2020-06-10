module Main where
import System.Environment

main = do putStrLn "Access-Control-Allow-Origin: *"
          putStrLn "Content-type: text/html\n\nHello world"
          v<- getEnvironment
          print v
