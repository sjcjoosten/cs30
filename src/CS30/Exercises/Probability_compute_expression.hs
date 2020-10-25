{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
module CS30.Exercises.Probability_compute_expression where
import CS30.Data
import CS30.Exercises.Data
import CS30.Exercises.Problems
import CS30.Exercises.SolutionChecker
import           Data.Aeson.TH 


probaEx :: ExerciseType
probaEx = exerciseType "ProbaCompute" "L?.?" "Probability : compute expression"
            choiceTreeList
            genQuestion
            genFeedback

-- randomSelect (Branch choiceTreeList)
genQuestion :: ([Field], Rational) -> Exercise -> Exercise
genQuestion (question, answer) ex = ex{eQuestion = question ++ [FFieldMath "answer"]}
