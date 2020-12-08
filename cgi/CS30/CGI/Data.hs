{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module CS30.CGI.Data where
import           Data.Aeson as JSON
import           Data.Aeson.TH
import           Instances.TH.Lift()

-- | How a client is linked to our application
data ClientLink
 = TempSession
     String -- ^ temporary session storage file
 | PermSession
     String -- ^ permanent session storage file
     String -- ^ user credentials
  deriving (Show)

isTemp :: ClientLink -> Bool
isTemp (TempSession _) = True
isTemp _ = False

dataFile :: ClientLink -> String
dataFile (TempSession x) = x
dataFile (PermSession x _) = x

data Authentication = Auth{aEmail::String,aUID::String,aRoles::[String],aGradeReport::Maybe GradeReporting}
  deriving Show
data GradeReporting = GradeR{aReportURL :: String, aSourceUID :: String, aOAuthKey::String}
  deriving Show

$(deriveJSON defaultOptions ''GradeReporting)
$(deriveJSON defaultOptions ''Authentication)
$(deriveJSON defaultOptions ''ClientLink)