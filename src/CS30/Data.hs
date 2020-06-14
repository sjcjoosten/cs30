{-# LANGUAGE DeriveLift      #-}
{-# LANGUAGE TemplateHaskell #-}
module CS30.Data where
import           Control.Applicative
import           Control.Monad.Trans.State.Lazy as StateT
import           Data.Aeson as JSON
import           Data.Aeson.TH
import qualified Data.Map as Map
import qualified Data.Text.Lazy as Text
import           Instances.TH.Lift()
import           Language.Haskell.TH.Syntax
import           Data.Void

-- table of content:
-- 1. Global constants
-- 2. Data types that go back and forth between client and server
-- 3. Data types for communicating to the client (potentially stored)
-- 4. Data types for communicating from the client (potentially stored)
-- 5. Data that is only stored on the server, never communicated
-- 6. Other data types?

-- * Global constants
globalDataDir :: FilePath
globalDataDir = $( do txt <- runIO$ readFile "data/dir"
                      [e|txt|] )
globalSalt :: Text.Text
globalSalt = $( do txt <- (runIO$ readFile "data/salt")
                   [e|Text.pack txt|] )

-- * Data types that go back and forth between client and server

-- | A potential action which the user may do with an exercise
--   For instance, Submit is used in tests (stores the answer given, hides the question)
--   Check is used in practice exercises (check if the answer is correct, get a next question)
--   TooHard can be used in practice too (skip question, get an easier one first)
data Action = Submit | Check | TooHard Int
  deriving (Show)

-- * Data types only for communicating to the client

-- | A field in an exercise (text, textfield, help button, etc..)
data Field = FText String -- text to be displayed
           | FFieldMath String -- fieldname to attach the response to
           | FValue{fvName::String,fvVal::Int} -- numeric value to be returned verbatim
           | FValueS{fvName::String,fvValS::String} -- like FValue, but a string
           | FMath String -- text to display as math
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

data ProblemOutcome
      = POCorrect -- ^ Full points, user answered correct
      | POIncorrect -- ^ No points, user answered incorrect
      -- -- | POSkip -- ^ No points, reduced penalty, user chose to skip
  deriving (Show, Lift, Eq)

-- | Exercises as sent to the client (via Json). Documentation describes the client's interpretation
data Exercise
      = Exercise{eTopic::String -- ^ Used as a header
                ,eQuestion::[Field] -- ^ Content of the question
                ,eActions::[Action] -- ^ Buttons under the question
                ,eHidden::[Field] -- ^ Meta-info (not displayed)
                }
  deriving (Show)

defaultExercise :: Exercise
defaultExercise = Exercise "" [] [] []

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
instance Semigroup Rsp where
  (<>) x y
   = mempty{rPages = rPages x <> rPages y
           ,rExercises = rExercises x <> rExercises y
           ,rSes = rSes x
           ,rCurrentPage = rCurrentPage x <|> rCurrentPage y
           ,rLogin = rLogin x <|> rLogin y
           ,rEcho = rEcho y
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

-- | After generating a problem instance, store relevant info on disk so we can ask it again or generate feedback
data LevelProblem
      = LevelProblem {lpPuzzelID :: Int
                     ,lpLevel :: Int -- how hard is the puzzel?
                     ,lpPuzzelGen :: [Int] -- how it was randomly generated
                     ,lpPuzzelQst :: Text.Text -- json encoded question (handler specific)
                     ,lpPuzzelSol :: Text.Text -- json encoded solution (handler specific)
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


data Authentication = Auth{aEmail::String,aUID::String,aRoles::[String]}
  deriving Show

-- Env is used for passing around global information
data Env = Env{eSes :: String -- session string (to be communicated back and forth)
              ,eMap :: Map.Map String [String] -- all query info
              ,eStorage :: FilePath -- where information is stored about current user
              }

$(deriveJSON defaultOptions ''Exercise)
$(deriveJSON defaultOptions ''ExResponse)
$(deriveJSON defaultOptions ''Rsp)
$(deriveJSON defaultOptions ''Splash)
$(deriveJSON defaultOptions ''ProblemResponse)
$(deriveJSON defaultOptions ''Env)
$(deriveJSON defaultOptions ''Authentication)
$(deriveJSON defaultOptions ''Field)
$(deriveJSON defaultOptions ''Action)
$(deriveJSON defaultOptions ''Page)
$(deriveJSON defaultOptions ''ProblemOutcome)
$(deriveJSON defaultOptions ''LevelProblem)
$(deriveJSON defaultOptions ''ProblemResolve)
$(deriveJSON defaultOptions ''LevelData)
$(deriveJSON defaultOptions ''SesData)
$(deriveJSON defaultOptions ''ClientLink)