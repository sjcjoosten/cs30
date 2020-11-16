{-# LANGUAGE DeriveFunctor      #-}
module CS30.Exercises.Data where
import           CS30.Data
import           Control.Monad.IO.Class (MonadIO(liftIO))
import           Data.Aeson
import qualified Data.Map as Map
import           System.Random (randomRIO)

data ExerciseType
  = ExerciseType{ etTag :: String
                , etMenu :: String
                , etTitle :: String
                , etChoices :: [ChoiceTree Value]
                , etGenEx :: Value -> Exercise -> Exercise
                , etGenAns :: Value -> Map.Map String String -> ProblemResponse -> ProblemResponse
                }

-- | Helperfunction to create an ExerciseType
exerciseType
      :: (ToJSON a, FromJSON a) -- We must be able to encode and decode in order to store the exercise properly
      => String -- ^ Identifier for URL
      -> String -- ^ Lecture this is part of
      -> String -- ^ Title of the exercise
      -> [ChoiceTree a] -- ^ list of exercises, which is supposed to be in increasing difficulty.
      -> (a -> Exercise -> Exercise) -- ^ A function that produces the exercise, first argument is the default exercise
      -> (a -> Map.Map String String -> ProblemResponse -> ProblemResponse) -- ^ Generate the Response popup from the exercise (a) and the client's response (a map with POST data)
      -> ExerciseType
exerciseType tg mn rn ct exGen fbGen
 = ExerciseType tg mn rn (map (fmap enc) ct) exGen' fbGen'
 where enc x = toJSON x
       exGen' :: Value -> Exercise -> Exercise
       exGen' t = case fromJSON t of
                    Success t' -> exGen t'
                    Error e -> error$ "Decoding error of the exercise ("++e++")"
       fbGen' :: Value -> Map.Map String String -> ProblemResponse -> ProblemResponse
       fbGen' a
         = case fromJSON a of
             Success a' -> fbGen a'
             Error e -> error$ "Decoding error of the exercise ("++e++")"

-- | Data-structure to take (perhaps randomly)
data ChoiceTree a = Node a | Branch [ChoiceTree a] deriving (Functor, Show, Eq)

-- | randomly return True or False
boolTree :: ChoiceTree Bool
boolTree = nodes [True,False]

-- | Super unsafe function! Requires its input to be a branch of at least two elements and removes the first.
tailBranch :: ChoiceTree a -> ChoiceTree a
tailBranch (Branch (_:lst@(_:_))) = Branch lst
tailBranch _ = error "TailBranch error"

-- | Easier way to create a choicetree at the inner level
nodes :: [a] -> ChoiceTree a
nodes = Branch . map Node

-- | Replacing inner ChoiceTree with another ChoiceTree
replace :: (t -> ChoiceTree a) -> ChoiceTree t -> ChoiceTree a
replace f (Node a) = f a
replace f (Branch lst) = Branch (map (replace f) lst)

-- | Get a random thing from a tree
randomSelect :: MonadIO m => ChoiceTree a -> m ([Int], a)
randomSelect (Branch lst)
  = do nr <- liftIO$ randomRIO (0,length lst-1)
       (is,a) <- randomSelect (lst!!nr)
       return (nr:is,a)
randomSelect (Node a) = return ([], a)

instance Monad ChoiceTree where
  return = Node
  (>>=) = flip replace
instance Applicative ChoiceTree where
  pure = return
  f <*> v = f >>= (\f' -> v >>= (\ v' -> return (f' v')))
