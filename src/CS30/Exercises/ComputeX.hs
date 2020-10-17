{-# LANGUAGE TemplateHaskell #-}

module CS30.Exercises.ComputeX where
import           CS30.Data
import           CS30.Exercises.Data (ExerciseType, exerciseType)
import           CS30.Exercises.ModularArithmetic.ModExercises(mods)
import qualified Data.Map as Map

computeQuestion :: ([Field], a) -> Exercise -> Exercise
computeQuestion (ques, _sol) ex = ex {eQuestion=[FText "Compute "] ++ ques ++ [FFieldMath "Answer"]} 

computeFeedback :: ([Field], [[String]]) -> Map.Map String String -> ProblemResponse -> ProblemResponse
computeFeedback _ mStrs rsp = case Map.lookup "Answer" mStrs of
                                            Just v -> markCorrect $ rsp{prFeedback = [FText ("You entered " ++ show v)] }
                                            Nothing -> error "Field must be filled"

modsEx :: ExerciseType
modsEx = exerciseType "Numbers" "L?.?" "Numbers: modulo p, compute X"
               mods
               computeQuestion
               computeFeedback
