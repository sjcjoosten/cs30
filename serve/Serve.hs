{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
import           CS30.Data
import           CS30.Static
import           CS30.Pages
import           CS30.Exercises (pages)
import           Control.Concurrent.MVar
import           Control.Monad.Trans.State.Lazy
import           Data.Aeson as JSON
import           Data.ByteString.UTF8 (toString)
import qualified Data.Map as Map
import           Data.Maybe
import           Network.HTTP.Types (status200, status404)
import           Network.Mime
import           Network.Wai
import           Network.Wai.Parse (parseRequestBody, lbsBackEnd) -- used to get post data
import           Network.Wai.Handler.Warp (run)
application :: MVar SesData -> Application
application ses request respond
 = let pi' = pathInfo request
   in case (pi',fileLookup pi') of
        (["~sjc","cs30","cs30.cgi"],_)
          -> do (params, _) <- parseRequestBody lbsBackEnd request
                let search = listToMaybe =<< Map.lookup "s" mp -- 's' is used when looking for a page (example: 'ex1')
                    exResponse = concat . maybeToList$ Map.lookup "ex" mp :: [String] -- 'ex' is used when responding to an exercise. json-encoded.
                    mp = Map.fromListWith (++) (map (\(x,v) -> (toString x,[toString v])) params)
                sesVal <- takeMVar ses
                (rsp,sesVal') <- runStateT (foldl (<>) mempty <$> sequenceA
                        (  [ handleExResponse rsp | rsp <- exResponse]
                        ++ [ populateEx search]
                        ++ [ return mempty{rPages = map mkPage pages}
                           | "page" <- concat . maybeToList $ Map.lookup "cAct" mp]
                        )) sesVal
                putMVar ses sesVal'
                respond $ responseLBS status200 [("Content-Type", "text/json")] (JSON.encode rsp)
        -- serve a static file, which has been found
        (_,Just v) -> respond $ responseBuilder status200 [("Content-Type", getMime pi')] v
        -- fallback: instead of crashing the server, generate a very simple 404 page
        (_,Nothing) -> do putStrLn ("Path not found: "<> show pi')
                          print request
                          respond $ responseLBS status404 [("Content-Type", "text/plain")] "Check the terminal to see the request that raised this 404"

getMime :: [FileName] -> MimeType
getMime [] = "text/html"
getMime (_:as@(_:_)) = getMime as
getMime [x] = defaultMimeLookup x

main :: IO ()
main = do currentExercises <- newMVar startData
          putStrLn "Please direct your browser to http://localhost:3000/"
          run 3000 (application currentExercises)
