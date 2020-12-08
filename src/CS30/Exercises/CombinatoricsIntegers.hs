{- CombinatoricsIntegers.hs

generates basic counting problems about integers, involving digit
placement, divisibility, and summing the digits

Here are some examples of possible questions:
● How many 5 digit positive integers are there such that it is divisible by 3, and that 9 is a
digit? (divisibility, digit placement)
● How many 6 digit positive integers are there such that the sum of the digits is at most
51? (summing the digits)
● How many 4 digit positive integers are there such that all digits are different? (custom
condition)

  
  TODO:
    1. find symbolic answers for all questions and use those
    2. write the symbolic answer as an expression and reuse the MathExpr evaluator
       from the `Cardinality' exercise
    3. require user input as expression?
-}

{-# LANGUAGE TemplateHaskell #-}
module CS30.Exercises.CombinatoricsIntegers (combinEx) where
import           CS30.Data
import           CS30.Exercises.Data
import qualified Data.Map as Map

combinEx :: ExerciseType
combinEx = exerciseType "CombinatoricsI" "L?.?" "Combinatorics: Integers" 
                        combins
                        genQuestion
                        genFeedback

{- divisibility -}

-- Generates a question about the divisibility of an n digit number
genDivisibility2,genDivisibility5 :: Int -> ChoiceTree ([Field], Int)
genDivisibility2 numDigit = nodes[( [FText $ show numDigit ++ " digit positive integers are there such that it is divisible by 2?"]
                                  , (9*10^(numDigit-2) * 5))]
genDivisibility5 numDigit = nodes[( [FText $ show numDigit ++ " digit positive integers are there such that it is divisible by 5?"]
                                  , (9*10^(numDigit-2) * 2))]

-- num digits -> num that have unique digits
solveUnique :: Int -> Int
solveUnique 0 = 1
solveUnique n
  = 9 * (solveRemainder 9 (n-1))
  where solveRemainder _ 0 = 1
        solveRemainder s r = s * solveRemainder (s-1) (r-1)

-- generates a question about the uniqueness of an n digit integer
genUnique :: Int -> ChoiceTree ([Field], Int)
genUnique n = Node ( [FText $ show n ++ " digit positive numbers are there such that all digits are different?"]
                   , (solveUnique n))

genSame :: Int -> ChoiceTree ([Field], Int)
genSame n = Node ( [FText $ show n ++ " digit positive numbers are there such that all digits are the same?"]
                 , 9)
{- digit placement -}

-- how many x-digit numbers contain a certain digit
-- solution stays the same despite the digit
numXHasDigit :: Int -> Int
numXHasDigit x = (9 * (10 ^ (x - 1))) - ((9 ^ (x - 1)) * 8)

genHasDigit :: (Int, Int) -> ChoiceTree ([Field], Int)
genHasDigit (n, d) = Node ( [FText $ show n ++ " digit positive numbers are there such that " ++ show d ++ " is a digit?"]
                          , (numXHasDigit n))

{- questions -}

combins :: [ChoiceTree ([Field], Int)]
combins = [ do n <- nodes [2..9]
               d <- nodes [1..9]
               genHasDigit (n, d)
          , Branch [mapN genUnique [2..9]
                   ,mapN genSame [2..9]]
          , do numDigit <- nodes [2..10]
               divisor  <- nodes [genDivisibility2, genDivisibility5]
               divisor numDigit
          ]
  where mapN f = Branch . map f

genQuestion:: ([Field],a) -> Exercise -> Exercise
genQuestion (quer, _solution) ex 
 = ex{ eQuestion = [ FText $"How many "] ++ quer ++ [FFieldMath "answer"]}


genFeedback :: ([Field],Int) -> Map.Map String String -> ProblemResponse -> ProblemResponse
genFeedback (_q, sol) mStrs rsp
  = case Map.lookup "answer" mStrs of 
      Just v -> if v == show sol then markCorrect $ rsp{prFeedback= [FText ("You entered " ++ show v)], prTimeToRead=60}
                else markWrong $ rsp{prFeedback= [FText ("You entered " ++ show v)], prTimeToRead=60}
      Nothing -> error "Answer field expected."
