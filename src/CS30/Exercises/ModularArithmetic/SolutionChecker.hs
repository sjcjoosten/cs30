{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.ModularArithmetic.SolutionChecker where
import           CS30.Data
import           CS30.Exercises.ModularArithmetic.ModExercises(ModEx, unwrap)
import           Data.Char
import           Data.Functor.Identity (Identity)
import qualified Data.Map as Map
import           Data.Void(Void)
import           Text.Megaparsec

type Parser = ParsecT Void String Identity

digit :: Parser Int
digit = cvt <$> satisfy isDigit
  where cvt d = fromEnum d - fromEnum '0'

digits :: Parser Int
digits = do ds <- some digit
            return (foldl1 shiftl ds)
         where shiftl m n = 10 * m + n

isTypo :: Int -> Int -> Int -> Bool
isTypo userAnswer storedAnswer modulus
    | decremented <= 0 = False
    | decremented == storedAnswer || incremented == storedAnswer = True
    | modulus == 10 = if userAnswer `div` 10 == storedAnswer then True else False
    | (length strStored) - (length strUser) == 1 = if strUser == (init strStored) then True else False
    | otherwise = False
    where decremented = userAnswer - 1
          incremented = userAnswer + 1
          strStored = show storedAnswer
          strUser = show userAnswer

genFeedback :: ModEx -> Map.Map String String -> ProblemResponse -> ProblemResponse
genFeedback modEx mStrs rsp = case Map.lookup "answer" mStrs of
                                Just v -> case parse digits "" val of
                                    Left _ -> markWrong $ rsp{prFeedback = [FText ("Please enter your answer as a non-negative integer")], prTimeToRead=60}
                                    Right userAnswer -> if userAnswer == storedAnswer
                                                        then markCorrect $ rsp{prFeedback = [FText ("You entered " ++ show userAnswer)], prTimeToRead=60}
                                                        else if isTypo userAnswer storedAnswer currentMod
                                                             then markCorrect $ rsp{prFeedback = [FText ("Partial credit given for typo " ++ show userAnswer ++ ". Correct answer " ++ show storedAnswer)], prTimeToRead=60}
                                                             else markWrong $ rsp{prFeedback = [FText ("You entered " ++ show userAnswer)], prTimeToRead=60}
                                    where notSpace = not . isSpace
                                          notSpaceReps = (/= '\\')
                                          val = filter (notSpace) $ filter(notSpaceReps) v
                                          (_, modulus, storedAnswer) = unwrap modEx   
                                          currentMod = read modulus :: Int 
                                Nothing -> error "Error: Server failed"
