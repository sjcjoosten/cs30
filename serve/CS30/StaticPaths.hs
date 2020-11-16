{-# LANGUAGE DeriveTraversable, DeriveLift #-}
{-# LANGUAGE OverloadedStrings #-}
module CS30.StaticPaths where
import qualified Data.ByteString as BS
import qualified Data.Map as Map
import qualified Data.Text as Text
import           Instances.TH.Lift ()
import           Language.Haskell.TH (Q, runIO)
import           Language.Haskell.TH.Syntax (addDependentFile,Lift)

-- guaranteed to return a value: returns the match furthest from the root
data PathTree k v
 = PathTree v !(Map.Map k (PathTree k v)) deriving (Functor,Foldable,Traversable,Lift)
pathTreeLookup :: (Ord k) => [k] -> [k] -> PathTree k a -> (a, [k], [k])
pathTreeLookup [] acc (PathTree v _) = (v,reverse acc,[])
pathTreeLookup (a:rm) acc (PathTree v m)
 = case Map.lookup a m of
     Nothing -> (v,reverse acc,a:rm)
     Just pt -> pathTreeLookup rm (a:acc) pt

get :: Ord k => [k] -> PathTree k a -> a
get lst pt = case pathTreeLookup lst [] pt of
               (v,_,_) -> v

-- | A list of files that will be statically linked to the executable by inlining them.
-- This means files are kept in memory while the program runs, giving a blazingly fast way of serving them.
-- The files in this list are currently a couple of megabytes large collectively,
-- if more files are needed, it is perhaps better to dynamically link them instead.
-- The code in 'Static.hs' is responsible for the static linking.
static_files :: PathTree Text.Text (Maybe Text.Text)
static_files
  = augment_paths "static" $
    file_tree (Just "/single.html")
              [ dir "js" [file "cs30.js", file "sortable.js", file "visnetwork.js"]
              , dir "mathquill"
                    [ file "mathquill-basic.css", file "mathquill-basic.js"
                    , file "mathquill.css", file "mathquill.js"
                    , file "mathquill.min.js"
                    , dir "font" [ file "Symbola-basic.eot"
                                 , file "Symbola-basic.ttf"
                                 , file "Symbola-basic.woff"
                                 , file "Symbola-basic.woff2"
                                 , file "Symbola.eot"
                                 , file "Symbola.otf"
                                 , file "Symbola.svg"
                                 , file "Symbola.ttf"
                                 , file "Symbola.woff"
                                 , file "Symbola.woff2"
                                 ]
                    ]
              , dir "css" [file "normalize.css", file "skeleton.css"]
              , dir "~sjc" [] -- don't serve anything in the sjc directory
              ]
  where
    dir :: Text.Text -> [(Text.Text, PathTree Text.Text (Maybe Text.Text))] -> (Text.Text, PathTree Text.Text (Maybe Text.Text))
    dir v lst = (v, file_tree Nothing lst)
    file :: Text.Text -> (Text.Text, PathTree Text.Text (Maybe Text.Text))
    file v = (v, file_tree (Just "") [])
    file_tree v elms = PathTree v (Map.fromList elms)

augment_paths :: Text.Text -> PathTree Text.Text (Maybe Text.Text) -> PathTree Text.Text (Maybe Text.Text)
augment_paths str (PathTree v mp)
 = PathTree (fmap (str <>) v) (Map.mapWithKey (\k -> augment_paths (str <> "/" <> k)) mp)

qReadFileBS :: Text.Text -> Q BS.ByteString
qReadFileBS fp
  = do let fp'::String
           fp' = Text.unpack fp
       addDependentFile fp'
       runIO $ BS.readFile fp'
