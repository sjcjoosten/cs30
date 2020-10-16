module CS30.Exercises.ProbWord (probBasicEx) where
import CS30.Data (Field(..),Exercise(..))
import CS30.Exercises.Data (ExerciseType,exerciseType)
import CS30.Exercises.ProbWord.Basics
import CS30.Exercises.ProbWord.SolutionChecker

probBasicEx :: ExerciseType
probBasicEx = exerciseType "ProbWordProblem" "L*.*" "Basic Probability" basicprob probQuer probFeedback

probQuer :: ([Field], a) -> Exercise -> Exercise
probQuer (quer, _solution) def
 = def{ eQuestion = [ FText $"Input "] ++quer
        }