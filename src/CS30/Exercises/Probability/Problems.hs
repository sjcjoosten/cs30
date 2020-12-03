{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
module CS30.Exercises.Probability.Problems where
import CS30.Data
import CS30.Exercises.Data
import GHC.Real -- for (%)
import CS30.Exercises.Probability.SolutionChecker ( showProb )

-- generate choiceTreeList contain several guestions
choiceTreeList :: [ChoiceTree ([Field], Rational)]
choiceTreeList = [ Branch [ nodes [andExerciseEasy a b | a <- proba , b <- proba]
                          , nodes [andThreeExerciseEasy a b c | a <- proba , b <- proba, c <- proba]
                          , nodes [orExerciseEasy a b | a <- proba , b <- proba]
                          ]
                 , nodes [deExerciseEasy a b | a <- proba , b <- proba]
                 , Branch [ nodes [andExerciseHard a b | a <- proba , b <- proba]
                          , nodes [orExerciseHard a b | a <- proba , b <- proba]
                          ]
                 ]

--generate questions based on two numbers
andExerciseEasy :: Rational -> Rational -> ([Field], Rational)
andExerciseEasy a b = ([FMath$ "Pr[A] =" ++ showProb(a), FText ", ", FMath$ "Pr[B] =" ++ showProb(b), 
                        FText ", Events A and B are independent. What is ", FMath "Pr[A \\wedge B]?"], (a*b))

andThreeExerciseEasy :: Rational -> Rational -> Rational -> ([Field], Rational)
andThreeExerciseEasy a b c = ([FMath$ "Pr[A] =" ++ showProb(a), FText ", ", FMath$ "Pr[B] =" ++ showProb(b), FMath$ "Pr[C] =" ++ showProb(c), 
                        FText ", Events A and B and C are independent. What is ", FMath "Pr[A \\wedge B \\wedge C]?"], (a*b*c))

orExerciseEasy ::  Rational -> Rational -> ([Field], Rational)
orExerciseEasy a b = ([FMath$ "Pr[A] =" ++ showProb(a), FText ", ", FMath$ "Pr[B] =" ++ showProb(b), 
                        FText ", Events A and B are independent. What is ", FMath "Pr[A \\vee B]?"], (a + b - a * b))

andExerciseHard :: Rational -> Rational -> ([Field], Rational)
andExerciseHard a b = ([FMath$ "Pr[A] =" ++ showProb(a), FText ", ", FMath$ "Pr[A \\wedge B] =" ++ showProb(a * b), 
                        FText ", Events A and B are independent. What is Pr[B]?"], b)

orExerciseHard :: Rational -> Rational -> ([Field], Rational)
orExerciseHard a b = ([FMath$ "Pr[A] =" ++ showProb(a), FText ", ", FMath$ "Pr[A \\vee B] =" ++ showProb(a + b - a * b), 
                        FText ", Events A and B are independent. What is Pr[B]?"], b)

-- dependent case
deExerciseEasy :: Rational -> Rational -> ([Field], Rational)
deExerciseEasy a b = ([FMath$ "Pr[A | B] =" ++ showProb(a), FText ", ", FMath$ "Pr[B] =" ++ showProb(b), 
                        FText ". What is ", FMath "Pr[A \\wedge B]?"], (a * b))

proba :: [Ratio Integer]
proba = [0.25, 0.5, 0.75, 0.2, 0.4, 0.6]
