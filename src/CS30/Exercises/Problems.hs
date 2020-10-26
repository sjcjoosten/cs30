{-# LANGUAGE InstanceSigs #-}
module CS30.Exercises.Problems where
import           CS30.Data
import           CS30.Exercises.Data
import GHC.Real -- for (%)

choiceTreeList :: [ChoiceTree ([Field], Rational)]
choiceTreeList = [Branch [nodes [andExerciseEasy a b | a <- proba , b <- proba]]
                    , Branch [nodes [andThreeExerciseEasy a b c | a <- proba , b <- proba, c <- proba]]
                    , Branch [nodes [orExerciseEasy a b | a <- proba , b <- proba]]
                    , Branch [nodes [andExerciseHard a b | a <- proba , b <- proba]]
                    , Branch [nodes [orExerciseHard a b | a <- proba , b <- proba]]]

--generate questions based on two numbers

andExerciseEasy :: Rational -> Rational -> ([Field], Rational)
andExerciseEasy a b = ([FText ("Pr[A] =" ++ show(a) ++ ", Pr[B] =" ++ show(b) ++ 
                                ", Events A and B are independent. What is Pr[A \\wedge B]?")], (a*b))

andThreeExerciseEasy :: Rational -> Rational -> Rational -> ([Field], Rational)
andThreeExerciseEasy a b c = ([FText ("Pr[A] =" ++ show(a) ++ ", Pr[B] =" ++ show(b) ++ ", Pr[C] =" ++ show(c) ++ 
                                ", Events A and B and C are independent. What is Pr[A \\wedge B \\wedge]?")], (a*b*c))                              

orExerciseEasy ::  Rational -> Rational -> ([Field], Rational)
orExerciseEasy a b = ([FText ("Pr[A] =" ++ show(a) ++ ", Pr[B] =" ++ show(b) ++ 
                            ", Events A and B are independent. What is Pr[A \\vee B]?")], (a + b - a * b))

andExerciseHard :: Rational -> Rational -> ([Field], Rational)
andExerciseHard a b = ([FText ("Pr[A] =" ++ show(a) ++ ", Pr[A∧B] =" ++ show(a * b) ++ 
                                            ", Events A and B are independent. What is Pr[B]?")], b)

orExerciseHard :: Rational -> Rational -> ([Field], Rational)
orExerciseHard a b = ([FText ("Pr[A] =" ++ show(a) ++ ", Pr[A \\vee B] =" ++ show(a + b - a * b) ++ 
                            ", Events A and B are independent. What is Pr[B]?")], b)



proba :: [Ratio Integer]
proba = [0.25, 0.5, 0.75, 0.2, 0.4, 0.6]
