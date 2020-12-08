module Main (main) where
import CS30.CGI.Pages
import CS30.CGI.Sessions
import CS30.Util
import Control.Exception (SomeException, catch)
import Data.ByteString.Builder (string7,toLazyByteString)
import System.Environment

main :: IO ()     
main = flip catch mkServerError $
       do uid <- toLazyByteString . string7 <$> getEnv "UNIQUE_ID" -- used for random nr generation
          inp <- getContents
          -- if inp == inp then return () else return ()
          v <- lookupEnv "QUERY_STRING"
          let qs = decodeQueryString inp
          case v of
            Nothing -> err500 "This script should be called via apache and an appropriate API, the environment variable QUERY_STRING needs to be set and apache would typically set it for us if this script is called with some GET-method data."
            Just _ -> handleResponse uid qs =<< handleRequest uid qs
            

mkServerError :: SomeException -> IO ()
mkServerError e = do putStrLn$ "Status: 500\nContent-type: text/plain\n\nCaught runtime error:"
                     putStrLn (show e)
