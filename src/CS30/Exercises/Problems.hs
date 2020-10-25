{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
module CS30.Exercises.Problems where
import           CS30.Data
import           CS30.Exercises.Data
import GHC.Real -- for (%)

data ProbEx = ProbAsRational [Field] Rational deriving Show

choiceTreeList :: [ChoiceTree ProbEx]
choiceTreeList = [Branch [ nodes [andExerciseEasy a b | a <- proba , b <- proba]
                         , nodes [orExerciseEasy a b | a <- proba , b <- proba]
                         , nodes [andExerciseHard a b | a <- proba , b <- proba]
                         , nodes [orExerciseHard a b | a <- proba , b <- proba]]]

--generate questions based on two numbers
andExerciseEasy :: Rational -> Rational -> ProbEx
andExerciseEasy a b = ProbAsRational [FText ("Pr[A] =" ++ show(a) ++ ", Pr[B] =" ++ show(b) ++ ", Events A and B are independent. What is Pr[A \\wedge B]?")] (a*b)

orExerciseEasy :: Rational -> Rational -> ProbEx
orExerciseEasy a b = ProbAsRational [FText ("Pr[A] =" ++ show(a) ++ ", Pr[B] =" ++ show(b) ++ ", Events A and B are independent. What is Pr[A \\vee B]?")] (a + b - a * b)

andExerciseHard :: Rational -> Rational -> ProbEx
andExerciseHard a b = ProbAsRational [FText ("Pr[A] =" ++ show(a) ++ ", Pr[A∧B] =" ++ show(a * b) ++ ", Events A and B are independent. What is Pr[B]?")] b

orExerciseHard :: Rational -> Rational -> ProbEx
orExerciseHard a b = ProbAsRational [FText ("Pr[A] =" ++ show(a) ++ ", Pr[A \\vee B] =" ++ show(a + b - a * b) ++ ", Events A and B are independent. What is Pr[B]?")] b

proba :: [Ratio Integer]
proba = [0.25, 0.5, 0.75, 0.2, 0.4, 0.6]
