{- CombinatoricsIntegers.hs

generates basic counting problems about integers, involving digit
placement, divisibility, and summing the digits

Here are some examples of possible questions:
● How many 5 digit positive integers are there such that it is divisible by 3, and that 9 is a
digit? (divisibility, digit placement)
● How many 6 digit positive integers are there such that the sum of the digits is at most
51? (summing the digits)
● How many 4 digit positive integers are there such that all digits are different? (custom
condition) -}

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
                        
{- summing digits -}

solveSum :: Int -> Int -> String
solveSum n sum_upperbound = show $ length $ [num | num <- (generateNDigitIntegers n), computeSumOfDigits(num) <= sum_upperbound]

-- Generate N-digit positive integers
generateNDigitIntegers :: Int -> [Int]
generateNDigitIntegers n = [smallest .. largest] 
                           where
                             smallest = 10 ^ (n - 1)
                             largest  = 10 ^ n - 1

-- Compute the sum of digits
computeSumOfDigits :: Int -> Int
computeSumOfDigits n = computeSumOfDigitsHelper n 0

computeSumOfDigitsHelper :: Int -> Int -> Int
computeSumOfDigitsHelper n curr_sum
  | n < 10     = curr_sum + n
  | otherwise  = computeSumOfDigitsHelper n' (curr_sum + digit)
  where
    (n', digit) = divMod n 10

-- Generates a question about sum of digits of an n-digit number
genSumDigits :: (Int, Int) -> ChoiceTree ([Field], String)
genSumDigits (n, sum_upperbound) = nodes[([FText $ show n ++ " digit positive integers are there such that the sum of the digits is at most "
  ++ show sum_upperbound ++ "?"], (solveSum n sum_upperbound))]

{- divisibility -}

-- Find how many x-digit numbers are divisible by y
numXDivisibleByY :: Int -> Int -> Int
numXDivisibleByY x y =
  let firstTerm = findFirstTerm x y
      lastTerm = 10 ^ x - 1
  in floor $ (fromIntegral :: Int -> Double) $ ((lastTerm - firstTerm) `div` y) + 1

-- Find the first x-digit number that is divisible by y
findFirstTerm :: Int -> Int -> Int
findFirstTerm x y
  | smallest `mod` y == 0           = smallest
  | otherwise                       = smallest + (y - smallest `mod` y)
  where
    smallest = 10 ^ (x - 1)

-- Generates a question about the divisibility of an n digit number
genDivisibility :: (Int, Int) -> ChoiceTree ([Field], String)
genDivisibility (numDigit, divisor) = nodes[([FText $ show numDigit ++ " digit positive integers are there such that it is divisible by "
  ++ show divisor ++ "?"], (show $ numXDivisibleByY numDigit divisor))]

{- custom condition -}

-- returns true if all elements in a list are unique
allUnique :: (Eq a) => [a] -> Bool
allUnique lst = case lst of
  [] -> True
  (x:xs) -> (notElem x xs) && allUnique xs

-- num digits -> num that have unique digits
solveUnique :: Int -> Int
solveUnique n = length ([lst | lst <- filter allUnique (map digits (generateNDigitIntegers n))])

-- breaks an integer (greater than zero) into a list of its digits
digits :: Int -> [Int]
digits 0 = []
digits n = mod n 10 : digits (div n 10)

-- generates a question about the uniqueness of an n digit integer
genUnique :: Int -> ChoiceTree ([Field], String)
genUnique n = nodes [([FText $ show n ++ " digit positive numbers are there such that all digits are different?"]
                    , (show $ solveUnique n))]

{- digit placement -}

-- returns true if a number contains the digit
hasDigit :: Int -> Int -> Bool
n `hasDigit` x = x `elem` digits n

-- how many x-digit numbers contain a certain digit
-- solution stays the same despite the digit
numXHasDigit :: Int -> Int
numXHasDigit x = (9 * (10 ^ (x - 1))) - ((9 ^ (x - 1)) * 8)

genHasDigit :: (Int, Int) -> ChoiceTree ([Field], String)
genHasDigit (n, d) = nodes [([FText $ show n ++ " digit positive numbers are there such that " ++ show d ++ " is a digit?"]
                    , (show $ numXHasDigit n))]

{- Combinations -}

-- Generative functions for combinations of questions
sumAndHasDigit :: (Int, Int, Int) -> ChoiceTree ([Field], String)
sumAndHasDigit (n, s, d) = nodes [([FText $ show n ++ " digit positive numbers are there such that the sum of the digits is at most "
                    ++ show s ++ ", and that " ++ show d ++ " is a digit?"]
                    , (show $ length $ [num | num <- (generateNDigitIntegers n), computeSumOfDigits(num) <= s, num `hasDigit` d]))]

divisibilityAndHasDigit :: (Int, Int, Int) -> ChoiceTree ([Field], String)
divisibilityAndHasDigit (n, divisor, digit) = nodes [([FText $ show n ++ " digit positive numbers are there such that it is divisible by "
                    ++ show divisor ++ ", and that " ++ show digit ++ " is a digit?"]
                    , (show $ length $ [num | num <- (generateNDigitIntegers n), num `mod` divisor == 0, num `hasDigit` digit]))]

sumAndDivisibility :: (Int, Int, Int) -> ChoiceTree ([Field], String)
sumAndDivisibility (n, s, divisor) = nodes [([FText $ show n ++ " digit positive numbers are there such that it is  divisible by "
                    ++ show divisor ++ ", and that the sum of the digits is at most " ++ show s ++ "?"]
                    , (show $ length $ [num | num <- (generateNDigitIntegers n), num `mod` divisor == 0, computeSumOfDigits(num) <= s]))]


{- questions -}

combins :: [ChoiceTree ([Field], String)]
combins = (map divisibilityAndHasDigit [(n, divisor, digit) | n <- [1..10], divisor <- [2..1000], digit <- [1..9]]) ++
         (map sumAndDivisibility [(n, s, d) | n <- [1..10], s <- [2..90], d <- [2..1000]]) ++
         (map sumAndHasDigit [(n, s, d) | n <- [1..10], s <- [2..90], d <- [1..9]]) ++
         (map genHasDigit [(n, d) | n <- [1..9], d <- [1..9]]) ++
         (map genUnique [2..9]) ++
         (map genSumDigits [(n,s) | n <- [2..9], s <- [1..90]]) ++
         (map genDivisibility [(numDigit, divisor) | numDigit <- [2..10], divisor <- [2..1000]])


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
