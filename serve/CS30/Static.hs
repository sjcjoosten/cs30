{-# LANGUAGE TemplateHaskell #-}
module CS30.Static (fileLookup) where
import qualified Data.Text as Text
import qualified Data.ByteString as BS -- strict
import qualified Data.ByteString.Builder as BS -- strict
import CS30.StaticPaths

fileData :: PathTree Text.Text (Maybe BS.ByteString)
fileData = $( do pt <- traverse (traverse qReadFileBS) static_files
                 [e|pt|] )

fileLookup :: [Text.Text] -> Maybe BS.Builder
fileLookup inp = BS.byteString <$> get inp fileData
