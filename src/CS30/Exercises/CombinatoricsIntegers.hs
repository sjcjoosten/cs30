{-Combinatorics: Integer Problems
Conceived by Sean
Your program should generate basic counting problems about integers, involving digit
placement, divisibility, summing the digits, and perhaps more of your choosing.
The general syntax looks like &quot;How many ____ integers are there such that ____ ?&quot; Here are
some examples:
Here are some examples:
● How many 5 digit positive integers are there such that it is divisible by 3, and that 9 is a
digit? (divisibility, digit placement)
● How many 6 digit positive integers are there such that the sum of the digits is at most
51? (summing the digits)
● How many 4 digit positive integers are there such that all digits are different? (custom
condition)
The numbers and problem-specific words used should be chosen dynamically, but be
careful that the problems are coherent when the pieces are put together.
The user should input an integer solution for any of these problems. Keep in mind that
combinatorics solutions explode as the parameters grow, so don&#39;t let them get too big.
The user&#39;s answer should simply be checked for equality against the integer solution.
As extra challenge, consider generating problems about positive rational numbers (i.e
any number of the form a/b, where a and b are positive integers).-}

{-# LANGUAGE TemplateHaskell #-}
module CS30.Exercises.CombinatoricsIntegers where
import           CS30.Data
import           CS30.Exercises.Data
import           Data.Aeson as JSON
import           Data.Aeson.TH
import qualified Data.Map as Map
import Debug.Trace


data CombinEx = CombinEx deriving Show
$(deriveJSON defaultOptions ''CombinEx)

combinEx :: ExerciseType
combinEx = exerciseType "Combinatorics" "L?.?" "Combinatorics: Integers" 
                        combins
                        genQuestion
                        genFeedback
                        

allDigits :: [Int]
allDigits = [1..9]

-- To Do
solve :: String
solve = "56"

combins :: [ChoiceTree ([Field], String)]
combins = [nodes [ ([FText "6 digit positive integers are there such that the sum of the digits is at most 51?"], solve)]
         , nodes [ ([FText "[This is Q2]"], solve)]
         , nodes [ ([FText "[This is Q3]"], solve)]
         , nodes [ ([FText "[This is Q4]"], solve)]
         , nodes [ ([FText "[This is Q5]"], solve)]]


genQuestion:: ([Field],a) -> Exercise -> Exercise
genQuestion (quer, _solution) ex 
 = ex{ eQuestion = [ FText $"How many "] ++ quer ++ [FFieldMath "answer"]}


genFeedback :: ([Field],String) -> Map.Map String String -> ProblemResponse -> ProblemResponse
genFeedback (_q, sol) mStrs rsp
  = trace ("genFeedback " ++ show mStrs) $
    case Map.lookup "answer" mStrs of 
      Just v -> if v == sol then markCorrect $ rsp{prFeedback= [FText ("You entered " ++ show v)], prTimeToRead=60}
                else markWrong $ rsp{prFeedback= [FText ("You entered " ++ show v)], prTimeToRead=60}
      Nothing -> error "Answer field expected."



-- genQuestion :: CombinEx -> Exercise -> Exercise
-- genQuestion _ ex = ex{eQuestion = [ FText "How many 6 digit positive integers are there such that the sum of the digits is at most 51?", FFieldMath "answer"]}

-- genFeedback :: CombinEx -> Map.Map String String -> ProblemResponse -> ProblemResponse
-- genFeedback _ mStrs rsp
--   = trace ("genFeedback " ++ show mStrs) $
--     case Map.lookup "answer" mStrs of 
--       Just v -> if v == "56" then markCorrect $ rsp{prFeedback= [FText ("You entered " ++ show v)], prTimeToRead=60}
--                 else markWrong $ rsp{prFeedback= [FText ("You entered " ++ show v)], prTimeToRead=60}
--       Nothing -> error "Answer field expected."