{-# OPTIONS_GHC -O2 #-}
-- | This module is for functions that are shared between the CGI interface and the SingleUser interface
module CS30.Util(uncalate, err500, decodeQueryString) where
import qualified Data.Map as Map
import           Network.HTTP.Base as CGI
import           Control.Monad.IO.Class (MonadIO(liftIO)) -- readFile
import           System.Exit

-- | Split on a character, used in Sessions.hs
uncalate :: Char -> String -> [String]
uncalate c = f
 where f = \s -> case dropWhile (== c) s of
                                "" -> []
                                s' -> w : f s''
                                      where (w, s'') =
                                             break (== c) s'
decodeQueryString :: String -> Map.Map String [String]
decodeQueryString = Map.fromListWith (++) . map treatPair . uncalate '&'

treatPair :: String -> (String, [String])
treatPair str
 = case break (== '=') str of
     (x,[]) -> (x, [])
     (x,_:v) -> (x, [CGI.urlDecode v])

err500 :: MonadIO m => String -> m a
err500 msg = liftIO $ do putStrLn$ "Status: 500\nContent-type: text/plain\n\n"++msg
                         exitFailure