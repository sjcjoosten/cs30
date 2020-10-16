module CS30.Exercises.Probability_compute_expression where
import CS30.Data
import CS30.Exercises.Data

data ProbEx = ProbEx deriving Show
probaEx :: ExerciseType
probaEx = exerciseType "ProbaCompute" "L?.?" "Probability : compute expression"
            [Node ProbEx]
            genQuestion
            genFeedback
