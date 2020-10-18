{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
module CS30.Exercises.Probability_compute_expression where
import CS30.Data
import CS30.Exercises.Data
import qualified Data.Map as Map
import           Data.Aeson.TH -- for deriveJSON
import Debug.Trace
-- import Data.Char (isNumber)
import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Void
import Data.Ratio
--import Prelude hiding (return,(>>=))

data ProbEx = ProbAsRational [Field] Rational deriving Show

$(deriveJSON defaultOptions ''ProbEx)

data NumericExpression = NENumber Rational | NEAdd NumericExpression NumericExpression -- stands for a + b where a and b are NumericExpression

numeric_value_parser :: Parser Rational
numeric_value_parser = do fst_part <- many digitChar
                          rmd <- rmdParser
                          case rmd of Nothing -> return (read fst_part % 1)
                                      Just v -> return ((read fst_part % 1) + (read v % (10 ^ length v)))
                      where rmdParser = (Just <$> (char '.' >> many digitChar)) <|> (return Nothing)
type Parser = Parsec Void String


-- example : 2
-- example : 0.75



probaEx :: ExerciseType
probaEx = exerciseType "ProbaCompute" "L?.?" "Probability : compute expression"
            choiceTreeList
            genQuestion
            genFeedback

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

-- randomSelect (Branch choiceTreeList)
genQuestion :: ProbEx -> Exercise -> Exercise
genQuestion (ProbAsRational question _) ex = ex{eQuestion = question ++ [FFieldMath "answer"]}

genFeedback :: ProbEx
                -> Map.Map String String
                -> ProblemResponse
                -> ProblemResponse
genFeedback (ProbAsRational _ answer)  mStrs rsp = trace ("genfeedback " ++ show mStrs) $
                    case Map.lookup "answer" mStrs of
                        Just v -> case parse numeric_value_parser "" v of
                                    Left e -> markWrong rsp{prFeedback = [FText ("You entered " ++ show v ++ ", parse error" ++ show e)]}
                                    Right userAnswer ->
                                        markCorrect $
                                        rsp{prFeedback = [FText ("You entered " ++ show userAnswer ++ ", the right answer is " ++ show answer)]} --todo change the time from 0 to sth
                        Nothing -> error "Answer field expected"
