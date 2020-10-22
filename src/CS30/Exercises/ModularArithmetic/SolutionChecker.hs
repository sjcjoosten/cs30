{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.ModularArithmetic.SolutionChecker where
import           CS30.Data
import           CS30.Exercises.ModularArithmetic.ModExercises(ModEx, unwrap)
import           Data.Char
import           Data.Functor.Identity (Identity)
import qualified Data.Map as Map
import           Data.Void(Void)
import           Debug.Trace
import           Text.Megaparsec

type Parser = ParsecT Void String Identity

digit :: Parser Int
digit = cvt <$> satisfy isDigit
  where cvt d = fromEnum d - fromEnum '0'

digits :: Parser Int
digits = do ds <- some digit
            return (foldl1 shiftl ds)
         where shiftl m n = 10 * m + n

-- Currently, this displays any answer as correct
stockGenFeedback :: ModEx -> Map.Map String String -> ProblemResponse -> ProblemResponse
stockGenFeedback _ mStrs rsp = trace ("genFeedback: " ++ show mStrs) $ 
                                 case Map.lookup "answer" mStrs of
                                    Just v -> markCorrect $ rsp{prFeedback = [FText ("You entered " ++ show v)], prTimeToRead=60}
                                    Nothing -> markWrong $ rsp{prFeedback = [FText "Error: Server failure."], prTimeToRead=60}

-- Accepts correct answers, rejects incorrect ones
genFeedback :: ModEx -> Map.Map String String -> ProblemResponse -> ProblemResponse
genFeedback modEx mStrs rsp = case Map.lookup "answer" mStrs of
                                Just v -> case parse digits "" val of
                                    Left _ -> markWrong $ rsp{prFeedback = [FText ("Please enter your answer as a non-negative integer")], prTimeToRead=60}
                                    Right answer -> if answer == ans
                                                    then markCorrect $ rsp{prFeedback = [FText ("You entered " ++ show answer)], prTimeToRead=60}
                                                    else markWrong $ rsp{prFeedback = [FText ("You entered " ++ show answer)], prTimeToRead=60}
                                    where notSpace = not . isSpace
                                          notSpaceReps = (/= '\\')
                                          val = filter (notSpace) $ filter(notSpaceReps) v
                                          (_, _, ans) = unwrap modEx    
                                Nothing -> error "Error: Server failed"
