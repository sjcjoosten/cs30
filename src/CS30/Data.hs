{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
module CS30.Data where
import           Control.Applicative
import           Control.Monad.Trans.State.Lazy as StateT
import           Data.Aeson as JSON
import           Data.Aeson.TH
import           Data.Char (toLower)
import qualified Data.Map as Map
import           Data.Semigroup as S
import           Data.Void
import           Instances.TH.Lift()
import           Language.Haskell.TH.Syntax

-- table of content:
-- 1. Data types that go back and forth between client and server
-- 2. Data types for communicating to the client (potentially stored)
-- 3. Data types for communicating from the client (potentially stored)
-- 4. Data that is only stored on the server, never communicated
-- 5. Other data types: they only exist on runtime, are not stored

-- * Data types that go back and forth between client and server

-- | A potential action which the user may do with an exercise
--   For instance, Submit is used in tests (stores the answer given, hides the question)
--   Check is used in practice exercises (check if the answer is correct, get a next question)
--   TooHard can be used in practice too (skip question, get an easier one first)
data Action = Submit | Check | TooHard Int
  deriving (Show)

-- * Data types only for communicating to the client

-- | A field in an exercise (text, textfield, help button, etc..)
data Field = FText String -- ^ text to be displayed
           | FMath String -- ^ text to display as math
           | FFieldMath String -- ^ fieldname to attach the response to
           | FFieldBool{ffOpt1::Field,ffOpt2::Field,ffDefault::(Maybe Bool),ffResponse::String} -- ^ Ask the user to choose between two options (first two arguments). Last argument is a name to attach the response to
           | FIndented {fIndentation::Int,fContent::[Field]} -- ^ An indented block of fields that sits on its own line(s)
           | FReorder {fvName :: String,fClusters::[[Field]]} -- ^ A list of elements that can be reordered. Response will be a list of Integers that 'read' can parse.
           | FChoice {fvName::String,fClusters::[[Field]]} -- ^ A list of elements to choose from, only one can be selected and a number is returned (as a string)
           | FValue{fvName::String,fvVal::Int} -- ^ numeric value to be returned verbatim
           | FValueS{fvName::String,fvValS::String} -- ^ like FValue, but a string
           | FTable [[Cell]] -- ^ Table, should be a rectangular, nonempty matrix
           | FNote String -- ^ text to be displayed in a small font on a separate line
           | FGraph{fgGraph::JSGraph, fgOptions::JSOptions} -- ^ graph displayed in a separate block
  deriving (Show)

data Cell = Cell Field
          | Header Field
  deriving (Show)

data JSGraph = JSGraph {jsgNodes :: [JSNode]
                       ,jsgEdges :: [JSEdge]}
  deriving (Show)
data JSEdge = JSEdge {jseFrom :: Int
                     ,jseTo :: Int
                     ,jseOptions :: JSOption}
  deriving (Show)
data JSNode = JSNode {jsnId :: Int
                     ,jsnOptions :: JSOption}
  deriving (Show)
data JSFont = JSFont {jsfMulti :: Maybe String}
  deriving (Show)
data JSOptions = JSOptions {jsoNodes :: JSOption
                           ,jsoEdges :: JSOption}
  deriving (Show)
data JSOption = JSOption
                     {jsoShape :: Maybe String
                     ,jsoSize :: Maybe Int
                     ,jsoLabel :: Maybe String
                     ,jsoFont :: Maybe JSFont}
  deriving (Show)

-- These are page references
-- They don't specify the actual page
data Page = Page{pId::String, pName::String}
  deriving (Show)

data ProblemResponse
      = ProblemResponse {prOutcome :: ProblemOutcome
                        ,prFeedback :: [Field] -- the corrected question
                        ,prTimeToRead :: Int -- suggested time to read in seconds (for autoplay feature)
                        }
  deriving (Show)

defaultProblemResponse :: ProblemResponse
defaultProblemResponse = ProblemResponse{prOutcome = POCorrect, prFeedback = [], prTimeToRead = 0}
markWrong,markCorrect,tryAgain :: ProblemResponse -> ProblemResponse
markWrong pr = pr{prOutcome = POIncorrect}
markCorrect pr = pr{prOutcome = POCorrect}
tryAgain pr = pr{prOutcome = POTryAgain}

data ProblemOutcome
      = POCorrect -- ^ Full points, user answered correct
      | POIncorrect -- ^ No points, user answered incorrect
      | POTryAgain -- ^ Not a valid answer, user needs to fix it
  deriving (Show, Lift, Eq)

-- | Exercises as sent to the client (via Json). Documentation describes the client's interpretation
data Exercise
      = Exercise{eTopic::String -- ^ Used as a header
                ,eQuestion::[Field] -- ^ Content of the question
                ,eActions::[Action] -- ^ Buttons under the question
                ,eHidden::[Field] -- ^ Meta-info (not displayed)
                ,eBroughtBy::[String] -- ^ Exercise was brought to you by: ... (UTF8)
                }
  deriving (Show)

defaultExercise :: Exercise
defaultExercise = Exercise "" [] [] [] []

data Splash = SplashPR ProblemResponse
            | XXX !Void -- there will be more fields later (sum-types generate different JSON structures, hence this field)
  deriving (Show)

-- Rsp is used to create a json response
data Rsp = Rsp{rPages::[Page] -- list of pages one can go to
              ,rExercises::[Exercise] -- list of exercises to display
              ,rSplash::Maybe Splash
              ,rSes::String -- session token to be repeated
              ,rCurrentPage::Maybe Page
              ,rLogin::Maybe String -- logged in as
              ,rEcho::Maybe String
              }
  deriving (Show)
instance Monoid Rsp where
  mempty = Rsp mempty mempty Nothing mempty Nothing Nothing mempty
instance S.Semigroup Rsp where
  (<>) x y
   = mempty{rPages = rPages x <> rPages y
           ,rExercises = rExercises x <> rExercises y
           ,rSes = rSes x
           ,rCurrentPage = rCurrentPage x <|> rCurrentPage y
           ,rLogin = rLogin x <|> rLogin y
           ,rEcho = rEcho x <|> rEcho y
           ,rSplash = rSplash x <|> rSplash y
           -- ,rDebug = rDebug y
           }

-- * Data types only for communicating from the client
-- | Client's response to an exercise, submitted for feedback
data ExResponse
      = ExResponse {cAction :: Action
                   ,cValue  :: Map.Map String String
                   ,exId    :: Int
                   ,exTag   :: String}
  deriving (Show)

-- * Data that is only stored on the server

-- | After generating a problem instance, store relevant info on disk so we can ask it again or generate feedback
data LevelProblem
      = LevelProblem {lpPuzzelID :: Int
                     ,lpLevel :: Int -- how hard is the puzzel?
                     ,lpPuzzelGen :: [Int] -- how it was randomly generated
                     ,lpPuzzelStored :: Value -- json encoded question (handler specific)
                     }
  deriving (Show, Lift)
-- | After a problem instance has been handled, we store how it was handled (potentially for overviews)
data ProblemResolve
      = ProblemResolve {lrProblem :: LevelProblem
                       ,lrUserAnswer :: Map.Map String String -- verbatim input from user (value of "cValue")
                       ,lrScore :: ProblemOutcome -- right or wrong
                       }
  deriving (Show, Lift)

-- | Current level information: what is the problem we are currently working on, 
-- | what were the past problems, how many stars did we get, etcetera..
-- | The function 'exerciseResponse' holds the logic for computing the current/next difficulty level,
-- | which is based on the bookkeeping of ldStreakPerLevel, ldWrongPerLevel and ldRightPerLevel.
data LevelData
      = LevelData{ldOpenProblem :: Maybe LevelProblem -- current problem we are working on. Don't provide a new problem until this one has been treated
                 ,ldPastProblems:: [ProblemResolve] -- ^ for bookkeeping or general interest, past problems in reverse order
                 ,ldCurrentLevel   :: Int -- ^ Increments and decrements depending on performance
                 ,ldMaxLevel       :: Int -- ^ Highest level attainable (for star calculations)
                 ,ldStreakPerLevel :: [Int] -- ^ once this gets to 10, the level is done permanently.
                 ,ldWrongPerLevel  :: [Integer] -- 
                 ,ldRightPerLevel  :: [Integer] -- proceed to next level once  right > 3 * wrong  (or if  streak>= 10)
                 ,ldStars          :: Int -- 0 to 3 stars, these should only increase
                 }
  deriving (Show, Lift)

defaultLevelData :: Int -> LevelData
defaultLevelData maxAt
      = LevelData{ldOpenProblem = Nothing
                 ,ldPastProblems= []
                 ,ldCurrentLevel  = 0
                 ,ldMaxLevel      = maxAt
                 ,ldStreakPerLevel= []
                 ,ldWrongPerLevel = []
                 ,ldRightPerLevel = []
                 ,ldStars         = -1
                 }

-- TODO: move to where used
-- At client's request, stars to be calculated (JavaScript side handling)
-- The current level and the performance based on the last 10 exercises determine the number of stars
calcStars :: LevelData -> Int -- 0, 1, 2 or 3 stars
calcStars ld
 = if nth (ldMaxLevel ld) (ldStreakPerLevel ld) >= 10 && pastStreakNr == 10 then 3
   else min 2 $ (if pastStreakNr == 10 then 1 else 0)
              + (if ldCurrentLevel ld == ldMaxLevel ld then 1 else 0)
              + (sum (map (\pr -> if lrScore pr == POCorrect then 1 else 0) (take 10 $ ldPastProblems ld)) `div` 7)
 where pastStreak (pr:lst) | lrScore pr == POCorrect = 1 + pastStreak lst
       pastStreak _ = 0
       pastStreakNr :: Int
       pastStreakNr = pastStreak (take 10 $ ldPastProblems ld)
       -- nth which defaults to zero
       nth :: (Num p) => Int -> [p] -> p
       nth i (_:as) | i > 0 = nth (i-1) as
       nth _ (h:_) = h
       nth _ [] = 0

-- | This data is passed around in the SesIO monad, contains the server-side data that is not meta-data
data SesData = SesData{ sdLevelData::Map.Map String LevelData
                      , sdEmail::Maybe String -- email address for traceability (if we have it), needed to forbid taking tests without login
                      }
  deriving (Show, Lift)

startData :: SesData
startData = SesData{sdLevelData=Map.empty,sdEmail=Nothing}

type SesIO = StateT SesData IO

instance Semigroup JSOption where
  JSOption a1 b1 c1 d1 <> JSOption a2 b2 c2 d2 = JSOption (a1 <|> a2) (b1 <|> b2) (c1 <|> c2) (d1 <|> d2)
instance Monoid JSOption where
  mempty = JSOption Nothing Nothing Nothing Nothing

instance Semigroup JSOptions where
  JSOptions a1 b1 <> JSOptions a2 b2 = JSOptions (a1 <> a2) (b1 <> b2)
instance Monoid JSOptions where
  mempty = JSOptions mempty mempty

-- code below is template haskell code
$(deriveJSON defaultOptions ''Exercise)
$(deriveJSON defaultOptions ''ExResponse)
$(deriveJSON defaultOptions ''Rsp)
$(deriveJSON defaultOptions ''Splash)
$(deriveJSON defaultOptions ''ProblemResponse)
$(deriveJSON defaultOptions ''Field)
$(deriveJSON defaultOptions ''Cell)
instance ToJSON JSNode where
  toJSON (JSNode id' settings)
    = case (object [("id", toJSON id')], toJSON settings) of
        (Object a, Object b) -> Object (a <> b)
        _ -> error "Expecting Objects in toJSON instance"
instance ToJSON JSEdge where
  toJSON (JSEdge from to settings)
    = case (object [("from", toJSON from),("to", toJSON to)], toJSON settings) of
        (Object a, Object b) -> Object (a <> b)
        _ -> error "Expecting Objects in toJSON instance"
instance FromJSON JSNode where
  parseJSON obj
    = withObject "JSNode" parseObj obj
    where parseObj v = JSNode <$> v .: "id" -- Note: this is using the 'overloadedString' extension to make "id" of type Text
                              <*> parseJSON obj
instance FromJSON JSEdge where
  parseJSON obj
    = withObject "JSNode" parseObj obj
    where parseObj v = JSEdge <$> v .: "from" -- Note: this is using the 'overloadedString' extension to make "id" of type Text
                              <*> v .: "to" -- Note: this is using the 'overloadedString' extension to make "id" of type Text
                              <*> parseJSON obj
$(deriveJSON defaultOptions{fieldLabelModifier = drop 3 . map toLower, omitNothingFields = True} ''JSGraph)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 3 . map toLower, omitNothingFields = True} ''JSOption)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 3 . map toLower, omitNothingFields = True} ''JSOptions)
$(deriveJSON defaultOptions{fieldLabelModifier = drop 3 . map toLower, omitNothingFields = True} ''JSFont)
$(deriveJSON defaultOptions ''Action)
$(deriveJSON defaultOptions ''Page)
$(deriveJSON defaultOptions ''ProblemOutcome)
$(deriveJSON defaultOptions ''LevelProblem)
$(deriveJSON defaultOptions ''ProblemResolve)
$(deriveJSON defaultOptions ''LevelData)
$(deriveJSON defaultOptions ''SesData)