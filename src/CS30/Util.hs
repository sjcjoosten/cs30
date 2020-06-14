{-# OPTIONS_GHC -O2 #-}
module CS30.Util (safeFilename, decodeQueryString, uncalate, err500) where
import qualified Data.Map as Map
import           Network.CGI as CGI -- readFile
import           System.Exit

decodeQueryString :: String -> Map.Map String [String]
decodeQueryString = Map.fromListWith (++) . map treatPair . uncalate '&'
  
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
treatPair :: String -> (String, [String])
treatPair str
 = case break (== '=') str of
     (x,[]) -> (x, [])
     (x,_:v) -> (x, [CGI.urlDecode v])
uncalate :: Char -> String -> [String]
uncalate c = f
 where f = \s -> case dropWhile (== c) s of
                                "" -> []
                                s' -> w : f s''
                                      where (w, s'') =
                                             break (== c) s'

err500 :: MonadIO m => String -> m a
err500 msg = liftIO $ do putStrLn$ "Status: 500\nContent-type: text/plain\n\n"++msg
                         exitFailure