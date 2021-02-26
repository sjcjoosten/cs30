{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module CS30.CGI.Sessions where
import           CS30.CGI.Data
import           CS30.CGI.Globals
import           CS30.CGI.Util
import           CS30.Data
import           CS30.Util
import           CS30.Exercises
import           CS30.Pages
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Trans.State.Lazy
import           Data.Aeson as JSON
import qualified Data.ByteString.Base64 as B64
-- import           Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Map as Map
import Data.List.Extra (splitOn)
import           Data.Maybe
-- import           Data.Text.Lazy.Encoding
import           Data.Time.Clock.POSIX
import           Network.HTTP.Base as CGI
import           Network.HTTP.Client
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Network.HTTP.Types.Header (hContentType)
import           Network.HTTP.Types.Status (statusCode)
import           Paths_cs30 (getDataDir)
import           System.Directory
import           System.Environment
import           System.Posix.Files
import           Text.Read (readMaybe)
import           Web.Authenticate.OAuth

makeReport :: String -> RunEnv -> ClientLink -> SesIO Rsp
makeReport s re clientLink
 = case JSON.decode (L8.pack s) of
     Nothing -> return mempty
     Just (v@ExResponse{})
       -> do let exRange = (exTag v,exId v,exId v + 1)
             case (cAction v, reSetScore re) of
               (Report,Just rst)
                 -> do rst exRange Nothing
                       return mempty{rSplash=Just (SplashFeedback "Your exercise history has been submitted for manual grading. Please inform your teacher about the issue you wish to report.")}
               (Report,Nothing)
                 -> return mempty{rSplash=Just (SplashFeedback ("To file a report about the current exercise, please mention: "++"/"++dataFile clientLink++rangeUri exRange))}
               _ -> return mempty

handleResponse :: L8.ByteString
               -> Map.Map String [String] -- POST data
               -> ClientLink -> IO ()
handleResponse uid mp clientLink
 = case splitOn "/" <$> search of
     Just ["?cs30.cgi", permId', page, start, end]
       | Just permId <- safeFilename permId'
       -> do Just initState <- decodeFileStrict (globalDataDir<>"/perm/"<>permId)
             case Map.lookup page (sdLevelData initState) of
               (Just ld) -> respond mempty{ rExercises = [ reconstructExercise page (lpLevel prob:lpPuzzelGen prob) (lpPuzzelStored prob) ans
                                                         | (ans,prob) <- (zip [Nothing] (maybeToList (ldOpenProblem ld))) ++ map (\pr -> (Just pr,lrProblem pr)) (ldPastProblems ld)
                                                         , lpPuzzelID prob >= read start && lpPuzzelID prob < read end
                                                         ]
                                    }
               Nothing -> respond mempty{ rSplash = Just (SplashFeedback "Could not find data")}
     _ -> do auth <- obtainAuth clientLink
             rsp <- runWithLink uid auth clientLink$
                   (\re -> foldl (<>) mempty{rLogin = aEmail <$> auth} <$> sequenceA
                           (  [ handleExResponse rsp | rsp <- exResponse ]
                           ++ [ makeReport rsp re clientLink | rsp <- exResponse]
                           ++ [ populateEx re search
                               ]
                           ++ [ return mempty{rPages = map mkPage pages}
                               | "page" <- concat . maybeToList $ Map.lookup "cAct" mp ]
                           ))
             respond rsp{rEcho = listToMaybe =<< Map.lookup "echo" mp, rSes = dataFile clientLink}
 where
    search = listToMaybe =<< Map.lookup "s" mp -- 's' is used when looking for a page (example: 'ex1')
    exResponse = concat . maybeToList$ Map.lookup "ex" mp :: [String] -- 'ex' is used when responding to an exercise. json-encoded.
    respond :: Rsp -> IO ()
    respond rsp = do putStrLn "Content-type: application/json\n"
                     L8.putStrLn (JSON.encode rsp)
                     return ()

-- reconstructExercise :: [Int] -> Value -> Maybe ProblemResolve -> IO Exercise

runWithLink :: L8.ByteString
            -> Maybe Authentication -- ^ Authentication information if any 
            -> ClientLink -> (RunEnv -> SesIO a) -> IO a
runWithLink uid auth cl f
 = do fe <- doesPathExist fName
      maybeInitState <- if fe then decodeFileStrict fName else return (Just startData)
      initState <- case maybeInitState of
                    Nothing | isTemp cl -> return startData -- not logged in, so just create a new session to overwrite the old
                      | otherwise -> liftIO . err500 $ "Your data on the server became corrupted, please ask Sebastiaan to fix this manually. Tell him: "<>fName
                    Just v -> return v
      (res, newState) <- runStateT (f runEnv{reSetScore = (postGrade <$> (aServer <$> auth) <*> return uid <*> (aGradeReport =<< auth))}) initState{sdEmail = aEmail <$> auth}
      encodeFile fName newState
      return res
 where fName = globalDataDir <> dataFile cl

postGrade :: String -> L8.ByteString -> GradeReporting -> SaveResult
postGrade serverURL uid repDetails puzR score -- Used https://lti.tools/saltire/tc to format 'payload'
 = do let key = aOAuthKey repDetails
      dataDir <- liftIO$ getDataDir
      secret <- liftIO$ C8.readFile (dataDir++"/data/"++key)
      p_request <- liftIO$ parseRequest (aReportURL repDetails)
      let uri = rangeUri puzR
      let payload = L8.pack "<?xml version = \"1.0\" encoding = \"UTF-8\"?>\n" <>
                    L8.pack "<imsx_POXEnvelopeRequest xmlns = \"http://www.imsglobal.org/services/ltiv1p1/xsd/imsoms_v1p0\">\n"<>
                    L8.pack "  <imsx_POXHeader>\n"<>
                    L8.pack "    <imsx_POXRequestHeaderInfo>\n"<>
                    L8.pack "      <imsx_version>V1.0</imsx_version>\n"<>
                    L8.pack "      <imsx_messageIdentifier>"<>uid<>L8.pack "</imsx_messageIdentifier>"<>
                    L8.pack "    </imsx_POXRequestHeaderInfo>\n"<>
                    L8.pack "  </imsx_POXHeader>\n"<>
                    L8.pack "  <imsx_POXBody><replaceResultRequest><resultRecord><sourcedGUID>\n"<>
                    L8.pack ("          <sourcedId>"<>aSourceUID repDetails<>"</sourcedId>\n")<>
                    L8.pack "        </sourcedGUID>\n"<>
                    L8.pack "        <result>\n"<>
                    (case score of
                      Nothing -> mempty
                      Just scr -> L8.pack "          <resultScore>\n"<>
                                  L8.pack "            <language>en-US</language>\n"<>
                                  L8.pack ("            <textString>"<>show scr<>"</textString>\n")<>
                                  L8.pack "          </resultScore>\n"
                    )<>
                    L8.pack "          <resultData>\n"<>
                    L8.pack ("              <url>"<>serverURL<>uri<>"</url>\n")<>
                    L8.pack "          </resultData>\n"<>
                    L8.pack "        </result>\n"<>
                    L8.pack "  </resultRecord></replaceResultRequest></imsx_POXBody>\n"<>
                    L8.pack "</imsx_POXEnvelopeRequest>"
      request <- liftIO$ signOAuth newOAuth{oauthConsumerKey=C8.pack key, oauthConsumerSecret=secret}
                           (Credential []) -- 'credential'. I think this is supposed to be a key/value pair. It's a list of pairs of bytestrings.
                           p_request{method=C8.pack "POST",secure=True,requestBody=RequestBodyLBS $ payload
                                    ,requestHeaders=[(hContentType, C8.pack "application/xml; charset=utf-8")]}
      mgr <- liftIO$ newManager tlsManagerSettings
      liftIO$ withResponse request mgr checkResult 
 where checkResult r | statusCode (responseStatus r) == 200 = return ()
         | otherwise
         = do body <- brConsume (responseBody r)
              err500 (show (responseStatus r) ++"\n\n"++show (responseHeaders r) ++ "\n\n" ++ concatMap C8.unpack body)



-- | Handles session cookies, calls authentication via oauth, and returns one or two file paths:
-- | either one for guests pointing to a temporary storage file in the tmp directory
-- | or two for logged in users, the first points to the credential-file and the second to the storage file in the perm directory.
-- | Also sets the cookie header.
handleRequest :: Map.Map String [String] -- POST data
              -> IO ClientLink
handleRequest mp
 = do createDirectoryIfMissing True (globalDataDir++"tmp/") -- temporary storage directory (may be deleted at will, will be re-created as needed)
      createDirectoryIfMissing True (globalDataDir++"perm/") -- permanent storage directory (like tmp, but has user ids associated, so should not be deleted)
      setFileMode (globalDataDir++"tmp/") accessModes 
      setFileMode (globalDataDir++"perm/") accessModes 
      serverURI <- lookupEnv "REQUEST_URI"
      serverHost <- lookupEnv "HTTP_HOST"
      let ses = listToMaybe =<< Map.lookup "ses" mp
      aut <- authenticate ((<>) <$> serverHost <*> (prefixCgi =<< serverURI)) -- own url (for refering to self in canvas)
                          (listToMaybe =<< Map.lookup "a" mp) -- all data sent through POST (missing if we are not in Canvas)
                          (listToMaybe =<< Map.lookup "h" mp) -- a hash of all data that is signed
      exists <- case ses of 
                 Just ('p':'e':'r':'m':'/':fn) | Just ses' <- safeFilename fn
                   -> (\x y -> if x && y then Just fn else Nothing) <$> doesFileExist (globalDataDir++"perm/"++ses') <*> doesFileExist (globalDataDir++"auth/"++ses')
                 _ -> return Nothing
      case (exists,ses, aut) of
           (Just ses',_,_) -> (return (PermSession ("perm/"<>ses') ("auth/"<>ses')))
           (_,_, Just auth)
             -> return (PermSession ("perm/" <> aUID auth) ("auth/" <> aUID auth))
           (_,Just ('t':'m':'p':'/':ses'), _) | Just tmpDir <- safeFilename ses' -- temporary sessions
             -> do return (TempSession ("tmp/"<>tmpDir))
           (_,Just ses', _) | Just tmpDir <- safeFilename ses' -- unique id generated by client for the first try
             -> do return (TempSession ("tmp/"<>tmpDir))
           (_,_,_) -> error ("CGI script was called in a way that does not allow it to store any progress: "++show ses)

prefixCgi :: String -> Maybe String
prefixCgi "cs30.cgi" = Just "?cs30.cgi"
prefixCgi (x:xs) = (x:) <$> prefixCgi xs
prefixCgi _ = Nothing


obtainAuth :: ClientLink -> IO (Maybe Authentication)
obtainAuth (TempSession _) = return Nothing
obtainAuth (PermSession _ usr)
 = do res <- decodeFileStrict (globalDataDir<>usr)
      case res of
        Nothing -> err500 "Authenticated user directory outdated. Please ask Sebastiaan what to do."
        Just auth -> return (Just auth)

-- get the session token we are supposed to use
authenticate :: Maybe String -> Maybe String -> Maybe String -> IO (Maybe Authentication)
authenticate serverURL str hash
 = case (oauth_key,oauth_timestamp) of
     (Just oauth_key',Just time_oa) ->
       do dataDir <- getDataDir
          secret <- L8.readFile (dataDir++"/data/"++oauth_key')
          let key = secret <> L8.pack "&"
          let c = B64.encode
                . L8.toStrict
                . SHA.bytestringDigest
                . SHA.hmacSha1 key
                . L8.pack <$> str
          if str == Nothing then err500 "Couldn't find data" else return ()
          if hash == Nothing then err500 "Couldn't find hash" else return ()
          time <- round <$> getPOSIXTime
          
          -- Legit scenario for the timeout error:
          --   I got this error after installing the environment on Canvas without installing the shared secret.
          --   The error for the missing key was a 'file not found' for readFile, and after fixing it I clicked the 'try again' button, and got the error below.
          -- Security risks for the timeout:
          --   ??
          if abs (time - time_oa) > 3100 then err500$ "Clocks seem out of sync (OAuth time: "++show time_oa++", Server time: "++show time++"). Fix is most likely to revisit this page from Canvas."
            else if c == (C8.pack <$> hash)
            then case userFn of
                   Nothing -> err500 "Could not get a user_id. This is perhaps an OAuth error; if you have linked this with a learning environment other than Canvas, there may be an easy fix."
                   Just fn ->
                    let fname = globalDataDir ++"auth/"++fn
                    in do createDirectoryIfMissing True (globalDataDir++"auth/")
                          setFileMode (globalDataDir++"auth/") accessModes
                          case res of
                            Nothing -> err500 "Error processing authentication information"
                            Just v -> do encodeFile fname v -- Store the most recent authentication data
                                         return (Just v)
            else err500$ "OAuth error: the secret used to sign the request does not seem to match our secret. \nThis is a setting."++show hash++show c++show key
     _ -> case mp of
            Nothing -> return Nothing
            Just _ -> err500 ("Missing or invalid OAuth fields "<> show (Map.lookup "oauth_consumer_key" =<< mp))
 where
  mp = decodeQueryString . CGI.urlDecode <$> str
  oauth_key = safeFilename =<< listToMaybe =<< Map.lookup "oauth_consumer_key" =<< mp
  oauth_timestamp = (readMaybe =<< listToMaybe =<< Map.lookup "oauth_timestamp" =<< mp)::Maybe Integer
  emailMaybe = listToMaybe =<< Map.lookup "lis_person_contact_email_primary" =<< mp
  roles = concatMap (uncalate ',' . CGI.urlDecode) <$> (Map.lookup "roles" =<< mp)
  userFn = safeFilename =<< (case lis_result_sourcedid of
                               Nothing -> do mp' <- mp
                                             uid <- Map.lookup "user_id" mp'
                                             CGI.urlEncode <$> listToMaybe uid
                               Just v -> Just (C8.unpack (B64.encode (C8.pack v))))
  lis_outcome_service_url = CGI.urlDecode <$> (listToMaybe =<< Map.lookup "lis_outcome_service_url" =<< mp)
  lis_result_sourcedid    = CGI.urlDecode <$> (listToMaybe =<< Map.lookup "lis_result_sourcedid"    =<< mp)
   -- credentials the user is trying to get
  res = Auth <$> emailMaybe <*> userFn <*> roles <*> return gradeReporting <*> ((\x y-> x++"/"++y) <$> serverURL <*> userFn)
  gradeReporting = GradeR <$> lis_outcome_service_url <*> lis_result_sourcedid <*> oauth_key

