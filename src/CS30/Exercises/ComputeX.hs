{-# LANGUAGE TemplateHaskell #-}

module CS30.Exercises.ComputeX where
import CS30.Data
import CS30.Exercises.Data (ExerciseType, exerciseType, ChoiceTree( Node ))
import qualified Data.Map as Map
import Data.Aeson.TH

data ComputeEx = ComputeEx deriving Show
$(deriveJSON defaultOptions ''ComputeEx)

computeX :: ExerciseType
computeX = exerciseType "Numbers" "L?.?" "Numbers: modulo p, compute X"
               [Node ComputeEx]
               computeQuestion
               computeFeedback

computeQuestion :: ComputeEx -> Exercise -> Exercise
computeQuestion _ ex = ex {eQuestion=[FText "Stub question", FFieldMath "Answer"]}

computeFeedback :: ComputeEx -> Map.Map String String -> ProblemResponse -> ProblemResponse
computeFeedback _ mStrs rsp = case Map.lookup "Answer" mStrs of
                                  Just v -> markCorrect $ rsp{prFeedback = [FText ("You entered " ++ show v)] }