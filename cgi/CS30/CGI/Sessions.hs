{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module CS30.CGI.Sessions where
import           CS30.CGI.Data
import           CS30.CGI.Globals
import           CS30.CGI.Util
import           CS30.Data
import           CS30.Util
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Control.Monad.Trans.State.Lazy
import           Data.Aeson as JSON
import           Data.ByteString.Base64 as B64
-- import           Data.ByteString.Base64.URL as B64URL
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Map as Map
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
postGrade serverURL uid repDetails score -- Used https://lti.tools/saltire/tc to format 'payload'
 = do let key = aOAuthKey repDetails
      dataDir <- getDataDir
      secret <- C8.readFile (dataDir++"/data/"++key)
      p_request <- parseRequest (aReportURL repDetails)
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
                    L8.pack "        <result><resultScore>\n"<>
                    L8.pack "            <language>en-US</language>\n"<>
                    L8.pack ("            <textString>"<>show score<>"</textString>\n")<>
                    L8.pack "        </resultScore><resultData>\n"<>
                    L8.pack ("                <text>Completed the required number of exercises correctly at: "<>serverURL<>"</text>\n")<>
                    L8.pack "              </resultData></result>\n"<>
                    L8.pack "  </resultRecord></replaceResultRequest></imsx_POXBody>\n"<>
                    L8.pack "</imsx_POXEnvelopeRequest>"
      request <- signOAuth newOAuth{oauthConsumerKey=C8.pack key, oauthConsumerSecret=secret}
                           (Credential []) -- 'credential'. I think this is supposed to be a key/value pair. It's a list of pairs of bytestrings.
                           p_request{method=C8.pack "POST",secure=True,requestBody=RequestBodyLBS $ payload
                                    ,requestHeaders=[(hContentType, C8.pack "application/xml; charset=utf-8")]}
      mgr <- newManager tlsManagerSettings
      withResponse request mgr checkResult 
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
      let ses = safeFilename =<< listToMaybe =<< Map.lookup "ses" mp
      aut <- authenticate ((<>) <$> serverHost <*> serverURI) -- own url (for refering to self in canvas)
                          (listToMaybe =<< Map.lookup "a" mp) -- all data sent through POST (missing if we are not in Canvas)
                          (listToMaybe =<< Map.lookup "h" mp) -- a hash of all data that is signed
      newLink <- case (ses, aut) of
                   (Just tmpDir, Nothing) -> return (TempSession ("tmp/"<>tmpDir))
                   (Nothing, Nothing) -> error "CGI script was called in a way that does not allow it to store any progress"
                   (_, Just auth)
                     -> return (PermSession ("perm/" <> aUID auth) ("auth/" <> aUID auth))
      -- Also save storage data?
      return newLink

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
            else err500$ "OAuth error: the secret used to sign the request does not seem to match our secret. \nThis is a setting."
     _ -> return Nothing
 where
  mp = decodeQueryString . CGI.urlDecode <$> str
  oauth_key = safeFilename =<< listToMaybe =<< Map.lookup "oauth_consumer_key" =<< mp
  oauth_timestamp = (readMaybe =<< listToMaybe =<< Map.lookup "oauth_timestamp" =<< mp)::Maybe Integer
  emailMaybe = listToMaybe =<< Map.lookup "lis_person_contact_email_primary" =<< mp
  roles = concatMap (uncalate ',' . CGI.urlDecode) <$> (Map.lookup "roles" =<< mp)
  userFn = safeFilename =<< (case lis_result_sourcedid of
                               Nothing -> listToMaybe =<< Map.lookup "user_id" =<< mp
                               Just v -> Just (C8.unpack (B64.encode (C8.pack v))))
  lis_outcome_service_url = CGI.urlDecode <$> (listToMaybe =<< Map.lookup "lis_outcome_service_url" =<< mp)
  lis_result_sourcedid    = CGI.urlDecode <$> (listToMaybe =<< Map.lookup "lis_result_sourcedid"    =<< mp)
   -- credentials the user is trying to get
  res = Auth <$> emailMaybe <*> userFn <*> roles <*> return gradeReporting <*> serverURL
  gradeReporting = GradeR <$> lis_outcome_service_url <*> lis_result_sourcedid <*> oauth_key

