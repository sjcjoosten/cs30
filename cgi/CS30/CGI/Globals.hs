{-# LANGUAGE TemplateHaskell #-}
module CS30.CGI.Globals where
import           Language.Haskell.TH.Syntax
import qualified Data.Text.Lazy as Text
-- import qualified Data.Text.Lazy.IO as Text (readFile)
import CS30.CGI.Util

-- * Global constants
globalDataDir :: FilePath
globalDataDir = $( do txt <- runIO$ readWithDefault "data/dir"
                      addDependentFile "data/dir"
                      [e|txt|] )
globalSalt :: Text.Text
globalSalt = $( do txt <- runIO$ readWithDefault "data/salt"
                   addDependentFile "data/salt"
                   [e|Text.pack txt|] )

