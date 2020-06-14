{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
module CS30.Sessions where
import           CS30.Data
import           CS30.Util
import           Control.Monad.Trans.State.Lazy
import           Data.Aeson as JSON
import           Data.ByteString.Base64.URL as B64
import qualified Data.ByteString.Char8 as C8
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Digest.Pure.SHA as SHA
import qualified Data.Map as Map
import           Data.Maybe
import           Data.Text.Lazy.Encoding
import           Data.Time.Clock.POSIX
import           Network.CGI as CGI
import           Network.CGI.Cookie as CGI
import           Paths_cs30
import           System.Directory
import           System.Directory (createDirectoryIfMissing,doesPathExist)
import           System.Environment
import           Text.Read (readMaybe)

runWithLink :: Maybe String -- ^ Email if any 
            -> ClientLink -> SesIO a -> IO a
runWithLink hasEmail cl f
 = do fe <- doesPathExist fName
      maybeInitState <- if fe then decodeFileStrict fName else return (Just startData)
      initState <- case maybeInitState of
                    Nothing | isTemp cl -> return startData -- not logged in, so just create a new session
                     | otherwise -> liftIO$ err500 "Your data on the server became corrupted, please ask Sebastiaan to fix this manually"
                    Just v -> return v
      (res, newState) <- runStateT f initState{sdEmail = hasEmail}
      encodeFile fName newState
      return res
 where fName = globalDataDir <> dataFile cl

-- | Handles session cookies, calls authentication via oauth, and returns one or two file paths:
-- | either one for guests pointing to a temporary storage file in the tmp directory
-- | or two for logged in users, the first points to the credential-file and the second to the storage file in the perm directory.
-- | Also sets the cookie header.
handleRequest :: BS.ByteString
              -> Map.Map String [String] -- POST data
              -> IO (String, ClientLink)
handleRequest uid mp
 = do cookies' <- lookupEnv "HTTP_COOKIE"
      let extract :: Maybe String -> Map.Map String String
          extract = Map.fromListWith const . CGI.readCookies . CGI.urlDecode . concat . maybeToList
           -- semigroup for Map prefers left operand values,
           -- we consistently prefer JavaScript's reported cookies over the browser's cookies
          cookies = extract (listToMaybe =<< Map.lookup "c" mp) <> extract cookies'
          sesName = case safeFilename =<< listToMaybe =<< (Map.lookup "ses" mp <> ((:[]) <$> Map.lookup "ses" cookies))
                      of Nothing -> C8.unpack . B64.encode . BS.toStrict . SHA.bytestringDigest . SHA.sha512
                                        $ encodeUtf8 globalSalt <> uid
                         Just v -> v
          fname = (globalDataDir <> "ses/" <> sesName)
      -- 7777777 seconds is 90 days, and chosen because it is the duration of a term
      putStrLn $ "Set-Cookie: ses="++sesName++"; Max-age:7777777"
      createDirectoryIfMissing True (globalDataDir++"ses/") -- session storage directory (may be deleted occasionally, but better to let Haskell do this so links to tmp are followed and also deleted)
      createDirectoryIfMissing True (globalDataDir++"tmp/") -- temporary storage directory (may be deleted at will, will be re-created as needed)
      createDirectoryIfMissing True (globalDataDir++"perm/") -- permanent storage directory (like tmp, but has user ids associated, so should not be deleted)
      aut <- authenticate (listToMaybe =<< Map.lookup "a" mp) (listToMaybe =<< Map.lookup "h" mp)
      isf <- doesPathExist fname
      curLink <- if isf then decodeFileStrict fname else return (Nothing :: Maybe ClientLink)
      newLink <- case (curLink, aut) of
                   (Just (TempSession tmpDir), Nothing) -> return (TempSession tmpDir)
                   (Just (PermSession pmDir usr), Nothing) -> return (PermSession pmDir usr)
                   (Nothing, Nothing) -> return (TempSession ("tmp/" <> sesName))
                   (_, Just auth)
                     -> do case curLink of
                             Just (TempSession tmpDir)
                               -> do isfP <- doesPathExist (globalDataDir <> "perm/" <> aUID auth)
                                     if isfP then removeFile (globalDataDir <> tmpDir)
                                             else renameFile (globalDataDir <> tmpDir) (globalDataDir <> "perm/" <> aUID auth)
                             _ -> return ()
                           return (PermSession ("perm/" <> aUID auth) ("auth/" <> aUID auth))
      -- We save the new link with our cookie:
      encodeFile fname newLink
      -- Also save storage data?
      return (sesName, newLink)

obtainEmail :: ClientLink -> IO (Maybe String)
obtainEmail (TempSession _) = return Nothing
obtainEmail (PermSession _ usr)
 = do res <- decodeFileStrict (globalDataDir<>usr)
      case res of
        Nothing -> err500 "Authenticated user directory outdated. Please ask Sebastiaan what to do."
        Just auth -> return (Just (aEmail auth))

{-
data ClientLink
 = TempSession
     String -- ^ temporary session storage file
 | PermSession
     String -- ^ permanent session storage file
     String -- ^ user credentials
  deriving (Show)
  -}
  
-- get the session token we are supposed to use
authenticate :: Maybe String -> Maybe String -> IO (Maybe Authentication)
authenticate str hash
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
          if abs (time - time_oa) > 31 then err500$ "Clocks seem out of sync (OAuth time: "++show time_oa++", Server time: "++show time++")"
            else if c == (C8.pack <$> hash)
            then case (safeFilename =<< listToMaybe =<< Map.lookup "user_id" =<< mp) of
                   Nothing -> err500 "Could not get a user_id. This is probably an OAuth error; if you have linked this with a learning environment other than Canvas, there may be an easy fix."
                   Just fn ->
                    let fname = globalDataDir ++"auth/"++fn
                    in do let emailMaybe = listToMaybe =<< Map.lookup "lis_person_contact_email_primary" =<< mp
                          let res = Auth <$> emailMaybe
                                         <*> pure fn
                                         <*> (concatMap (uncalate ',' . CGI.urlDecode) <$> (Map.lookup "roles" =<< mp))
                          createDirectoryIfMissing True (globalDataDir++"auth/")
                          case res of
                            Nothing -> err500 "Error processing authentication information"
                            Just v -> do encodeFile fname v -- Store the most recent authentication data
                                         return (Just v)
            else err500 "OAuth error: the secret used to sign the request does not seem to match our secret. This is a setting."
     _ -> return Nothing
 where
  mp = decodeQueryString . CGI.urlDecode <$> str
  oauth_key = safeFilename =<< listToMaybe =<< Map.lookup "oauth_consumer_key" =<< mp
  oauth_timestamp = (readMaybe =<< listToMaybe =<< Map.lookup "oauth_timestamp" =<< mp)::Maybe Integer

{-
context_id=bfb1a48c40e42e5a80a8f1c60f27671ce4e23601
context_label=COSC.030-SU20
context_title=Discrete%20Mathematics%20in%20Computer%20Science%20%28SU20%29
custom_canvas_enrollment_state=active
ext_roles=urn%3Alti%3Ainstrole%3Aims%2Flis%2FInstructor%2Curn%3Alti%3Arole%3Aims%2Flis%2FInstructor%2Curn%3Alti%3Arole%3Aims%2Flis%2FLearner%2FNonCreditLearner%2Curn%3Alti%3Arole%3Aims%2Flis%2FMentor%2Curn%3Alti%3Asysrole%3Aims%2Flis%2FUser
  .. = urn:lti:instrole:ims/lis/Instructor,urn:lti:role:ims/lis/Instructor,urn:lti:role:ims/lis/Learner/NonCreditLearner,urn:lti:role:ims/lis/Mentor,urn:lti:sysrole:ims/lis/User
launch_presentation_document_target=iframe
launch_presentation_locale=en
launch_presentation_return_url=https%3A%2F%2Fcanvas.dartmouth.edu%2Fcourses%2F40746%2Fexternal_content%2Fsuccess%2Fexternal_tool_redirect
lis_person_contact_email_primary=Sebastiaan.Joosten%40Dartmouth.edu
lti_message_type=basic-lti-launch-request
lti_version=LTI-1p0
oauth_callback=about%3Ablank
oauth_consumer_key=osinsumaspejdeetrogumwaeghif
oauth_nonce=fFW8tHA3smBwJMXgX6dxWA0cP9oiRfNb7HSHfJgK2Q
oauth_signature_method=HMAC-SHA1
oauth_timestamp=1591884245
oauth_version=1.0
resource_link_id=8b5b3318f8920ff61f4b415a58c1f586dfbb0967
resource_link_title=CS30
roles=Instructor
tool_consumer_info_product_family_code=canvas
tool_consumer_info_version=cloud
tool_consumer_instance_contact_email=notifications%40instructure.com
tool_consumer_instance_guid=7db438071375c02373713c12c73869ff2f470b68.dartmouth.instructure.com
tool_consumer_instance_name=Dartmouth%20College
user_id=478b3e688be3d80d388ad79cecff0514c8644321
-}
