module CS30.CGI.Pages where
import           CS30.CGI.Data
import           CS30.CGI.Sessions
import           CS30.Data
import           CS30.Exercises
import           CS30.Pages
import           Data.Aeson as JSON
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Map as Map
import           Data.Maybe

handleResponse :: L8.ByteString
               -> Map.Map String [String] -- POST data
               -> (String, ClientLink) -> IO ()
handleResponse uid mp (sesnr, clientLink)
 = do auth <- obtainAuth clientLink
      rsp <- runWithLink uid auth clientLink$
             (\re ->
             let search = listToMaybe =<< Map.lookup "s" mp -- 's' is used when looking for a page (example: 'ex1')
                 exResponse = concat . maybeToList$ Map.lookup "ex" mp :: [String] -- 'ex' is used when responding to an exercise. json-encoded.
             in foldl (<>) mempty{rSes=sesnr,rLogin = aEmail <$> auth} <$> sequenceA
                     (  [ handleExResponse rsp | rsp <- exResponse]
                     ++ [ populateEx re search
                        ]
                     ++ [ return mempty{rPages = map mkPage pages}
                        | "page" <- concat . maybeToList $ Map.lookup "cAct" mp]
                     ))
      respond rsp{rEcho = listToMaybe =<< Map.lookup "echo" mp}
 where
    respond :: Rsp -> IO ()
    respond rsp = do putStrLn "Content-type: application/json\n"
                     L8.putStrLn (JSON.encode rsp)
                     return ()
