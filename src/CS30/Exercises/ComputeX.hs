{-# LANGUAGE TemplateHaskell #-}

module CS30.Exercises.ComputeX where
import           CS30.Data
import           CS30.Exercises.Data (ExerciseType, exerciseType)
import           CS30.Exercises.ModularArithmetic.ModExercises(ModEx, unwrap, mods)
import qualified Data.Map as Map
import           Debug.Trace

-- Displays the list of questions
genQuestion :: ModEx -> Exercise -> Exercise
genQuestion modEx ex = ex {eQuestion = [ FText "Compute " ] ++ques++ 
                              [ FText (". Give your answer as a non-negative number less than " ++modulus++ "."), FFieldMath "answer" ] } 
                              where (ques, modulus, _) = unwrap modEx

-- Currently, this displays any answer as correct
genFeedback :: ModEx -> Map.Map String String -> ProblemResponse -> ProblemResponse
genFeedback _ mStrs rsp = trace ("genFeedback: " ++ show mStrs) $ 
                                 case Map.lookup "answer" mStrs of
                                    Just v -> markCorrect $ rsp{prFeedback = [FText ("You entered " ++ show v)], prTimeToRead=60}
                                    Nothing -> markWrong $ rsp{prFeedback = [FText "Error: Server failure."], prTimeToRead=60}

modsEx :: ExerciseType
modsEx = exerciseType "Numbers" "L?.?" "Modulo p, compute X"
               mods
               genQuestion
               genFeedback
