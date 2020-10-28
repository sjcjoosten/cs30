{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
module CS30.Exercises.Probability where
import CS30.Data
import CS30.Exercises.Data
import CS30.Exercises.Probability.Problems
import CS30.Exercises.Probability.SolutionChecker

probaEx :: ExerciseType
probaEx = exerciseType "ProbaCompute" "L?.?" "Probability : compute expression"
            choiceTreeList
            genQuestion
            genFeedback

-- randomSelect (Branch choiceTreeList)
genQuestion :: ([Field], Rational) -> Exercise -> Exercise
genQuestion (question, _) ex = ex{eQuestion = question ++ [FFieldMath "answer"]}
