module CS30.Exercises.ProbWord (probBasicEx, probExpectEx) where
import CS30.Data (Field(..),Exercise(..))
import CS30.Exercises.Data (ExerciseType,exerciseType)
import CS30.Exercises.ProbWord.Basics
import CS30.Exercises.ProbWord.ExpectedValue
import CS30.Exercises.ProbWord.SolutionChecker

probBasicEx :: ExerciseType
probBasicEx = exerciseType "BasicProb" "L*.*" "Basic Probability" basicprob probQuer probFeedback

probExpectEx :: ExerciseType
probExpectEx = exerciseType "ExpectedValue" "L*.*" "Expected Value" expectprob probQuer probExpectFeedback 

probQuer :: Real a => ([Field], a) -> Exercise -> Exercise
probQuer (quer, _solution) def
 = def{ eQuestion = [ FText $"If "] ++quer ++  [FText "\nYou may give your answer in decimal, percentage, or fractional form, as appropriate."]            
       , eBroughtBy = ["Fei Guo", "Kyle Bensink"]
        }