{-# OPTIONS_GHC -O2 #-}
module CS30.CGI.Util (safeFilename, readWithDefault) where
import Control.Exception (try)
import System.Directory (createDirectory)

-- | checking if the filename (which the user sent us) is a proper filename
safeFilename :: String -> Maybe String
safeFilename str
 = case nonDots of
     [] -> Nothing
     x -> if all isSafe x then Just str else Nothing
   where
    nonDots = filter (/= '.') str
    isSafe c
          = if (c <= 'Z') then
              if (c <= '9')
              then c >= '0' || c == '-'
              else c >= 'A' || c == '='
            else
              if (c < 'a') then c=='_'
              else c <= 'z'


-- | For dealing with globals
readWithDefault :: String -> IO String
readWithDefault str = try (readFile str) >>= warnAndDefault
  where cru :: Either IOError a -> IO ()
        cru _ = return () -- const return unit
        warnAndDefault :: Either IOError String -> IO String
        warnAndDefault (Left e)
          = do print e
               putStrLn$ "As the file "++str++" could not be read, we attempt to create a new file with a default value of "++show str++"."
               putStrLn$ "If you are installing this on a production server, this will need to be fixed!"
               try (createDirectory "data") >>= cru
               writeFile str str
               return str
        warnAndDefault (Right v)
          | v == str = do putStrLn$ "Warning: The default value for "++v++" is used, you might wish to change the contents of this file to match your setup."
                          return v
          | otherwise = return v
