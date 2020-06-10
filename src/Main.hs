{-# LANGUAGE TemplateHaskell #-}
module Main (main) where
import           Control.Exception
import           Data.Aeson as JSON
import           Data.Aeson.TH
import           Data.ByteString.Base64.URL as B64
import           Data.ByteString.Builder (string7,toLazyByteString)
import qualified Data.ByteString.Char8 as Char8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.ByteString.Lazy as BS
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Map as Map
import           Data.Maybe
import qualified Data.Text as Text
import qualified Data.Text.Lazy as Text (fromStrict)
import qualified Data.Text.IO as Text
import           Data.Text.Lazy.Encoding
import           Network.CGI as CGI -- readFile
-- import qualified Data.ByteString.UTF8 as UTF8 -- from utf8-string
import           Network.CGI.Cookie as CGI
-- hexadecimal output via 'showDigest'
import           Paths_cs30 as Paths -- bytestringDigest . sha512 . 
import           System.Directory (createDirectoryIfMissing)
import           System.Environment
import           System.Exit (die)
type Session = (String,Ses) -- session id and session info

-- Ses is used for the stored session information
-- It is stored in a file whose name is described in Session
data Ses = Ses
  deriving (Show)
data Exercise = Exercise
  deriving (Show)
data Page = Page
  deriving (Show)

-- Env is used for passing around global information
data Env = Env{eDataDir::String
              ,eSalt::Text.Text
              ,eSes::Session
              ,eMap::Map.Map String [String]
              ,eCookies::Map.Map String String}
-- Rsp is used to create a json response
data Rsp = Rsp{rPages::[Page] -- list of pages one can go to
              ,rExercises::[Exercise] -- list of exercises to display
              ,rSes::String -- session token to be repeated
              }
  deriving (Show)
$(deriveJSON defaultOptions 'Page)
$(deriveJSON defaultOptions 'Exercise)
$(deriveJSON defaultOptions 'Ses)
$(deriveJSON defaultOptions 'Env)
$(deriveJSON defaultOptions 'Rsp)

err500 :: String -> IO ()
err500 msg = die$ "Status: 500\nContent-type: text/plain\n\n"++msg

respond :: Rsp -> IO ()
respond rsp = do putStrLn "Status: 200\nContent-type: application/json\n\n"
                 L8.putStrLn (JSON.encode rsp)

main :: IO ()     
main = flip catch mkServerError $
       do dataDir <- getDataDir
          uid <- toLazyByteString . string7 <$> getEnv "UNIQUE_ID" -- used for random nr generation
          salt <- Text.readFile$ dataDir++"/data/salt" -- same
          v <- lookupEnv "QUERY_STRING"
          case v of
            Nothing -> err500 "This script should be called via apache and an appropriate API, the environment variable QUERY_STRING needs to be set and apache would typically set it for us if this script is called with some GET-method data."
            Just str
              -> case Map.fromListWith (++) . map treatPair . uncalate '&' $ str of
                   mp -> handleRequest dataDir (salt,uid) mp
mkServerError :: SomeException -> IO ()
mkServerError e = do putStrLn$ "Status: 500\nContent-type: text/plain\n\n"
                     putStrLn (show e)

handleRequest :: String -> (Text.Text, BS.ByteString) -> Map.Map String [String] -> IO ()
handleRequest dataDir uid mp
 = do cookies' <- lookupEnv "HTTP_COOKIE"
      let extract :: Maybe String -> [(String, String)]
          extract = CGI.readCookies . CGI.urlDecode . concat . maybeToList
          cookies = Map.fromListWith const
                    (extract (listToMaybe =<< Map.lookup "c" mp)
                     <> extract cookies')
      createDirectoryIfMissing True (dataDir++"/ses/") -- session directory
      ses <- case safeFilename =<< ((listToMaybe =<< Map.lookup "ses" mp) <>
                                   Map.lookup "ses" cookies) of
                Nothing -> newSession dataDir uid
                Just v -> findSession dataDir uid v
      withSession (Env dataDir (fst uid) ses mp cookies)

-- checking if the filename (which the user sent us) is a proper filename
safeFilename :: String -> Maybe String
safeFilename str
 = if all isSafe str then Just str
   else Nothing
   where isSafe c
          = if (c <= 'Z') then
              if (c <= '9')
              then c >= '0' || c == '-'
              else c >= 'A' || c == '='
            else
              if (c < 'a') then c=='_'
              else c <= 'z'

withSession :: Env -> IO ()
withSession env
 = do let dataDir = eDataDir env
      let (sesnr,ses) = eSes env
      let sesData = Ses -- based on ses, but updated
      -- 7777777 seconds is 90 days, and chosen because it is the duration of a term
      putStrLn $ "Set-Cookie: ses="++sesnr++"; Max-age:7777777"
      encodeFile (dataDir++"/ses/"++sesnr) sesData -- don't write data until after processing
      respond (Rsp{rPages=[],rSes=sesnr,rExercises=[]})

newSession :: String -> (Text.Text, BS.ByteString) -> IO (String, Ses)
newSession _dataDir (salt,uid)
 = return (sesnr, Ses) -- return empty session
 where sesnr = Char8.unpack (B64.encode (BS.toStrict (SHA.bytestringDigest (SHA.sha512 (encodeUtf8 (Text.fromStrict salt)<>uid)))))

findSession :: FilePath
            -> (Text.Text, BS.ByteString)
            -> FilePath -> IO (String, Ses)
findSession dataDir pr v
 = do r <- eitherDecodeFileStrict (dataDir <> "/ses/" <> v)
      case r of
        Left _msg -> newSession dataDir pr
        Right ses -> return (v,ses)

treatPair :: String -> (String, [String])
treatPair str
 = case break (== '=') str of
     (x,[]) -> (x, [])
     (x,_:v) -> (x, [v])
uncalate :: Char -> String -> [String]
uncalate c = f
 where f = \s -> case dropWhile (== c) s of
                                "" -> []
                                s' -> w : f s''
                                      where (w, s'') =
                                             break (== c) s'
