{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.LogicExpr.CreativeDisplay (logicWrongStepEx) where
import CS30.Exercises.LogicExpr.Parser
import CS30.Exercises.LogicExpr.Proof
import CS30.Exercises.LogicExpr.Display
import CS30.Data
import CS30.Exercises.Data
import qualified Data.Map as Map
import Debug.Trace

logicWrongStepEx :: ExerciseType
logicWrongStepEx
  = exerciseType "RewritingExpressionsHarder" "L?.?"
                 "Logic: rewriting expressions (harder version)" 
                 [(generateRandFaultyEx 3), (generateRandFaultyEx 5)]
                 generateExercise 
                 generateFeedback

-- Function for generating a random faulty proof from p, q, r, ^, ∨, ¬, true, and false.
generateRandFaultyEx :: Int -> ChoiceTree (LogicExpr, [[Field]], [Bool])
generateRandFaultyEx n
  = do e <- genRandEx n
       let expr = Neg e
       let proof = genFaultyProof expr
       let (fields, values) = constructFields proof
       return (expr, fields, values)

-- variant of map that passes each element's index as a second argument to f
-- https://stackoverflow.com/questions/16191824/index-of-element-in-list-in-haskell
mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

constructFields :: Proof Bool -> ([[Field]], [Bool])
constructFields proof = (mapInd showStep steps, map getVal steps)
  where steps = getSteps proof
        getSteps (Proof _ s)  = s
        showStep (_, ex) i = [ FMath "=", FIndented 1 [FMath $ show ex], FChoice (show i) [[FText "True"], [FText "False"]] ]
        getVal (val, _) = val

-- Function for the actual text displayed to the client.
generateExercise :: (LogicExpr, [[Field]], [Bool]) -> Exercise -> Exercise
generateExercise (expr, steps, sol) exercise 
  = trace (show sol) $ exercise{eQuestion = [ FText $"Here is an example proof, can you identify the incorrect steps?"
                         , FIndented 1 [FMath $ show expr] 
                         , FTable ((map . map) Cell steps)]
      , eBroughtBy = ["Tyler", "Fei"] }

-- Function for generating feedback and displaying it to the user. 
generateFeedback :: (LogicExpr, [[Field]], [Bool]) -> Map.Map String String -> ProblemResponse -> ProblemResponse
generateFeedback (_, _, sol) rsp pr 
  = case and $ zipWith compare' [0..((length sol) - 1)] sol of
      True -> markCorrect pr{prFeedback=[ FText "Your selections were correct. The correct answer was: ",FText display]}
      False -> markWrong pr{prFeedback=[ FText "Your selections were incorrect. The correct answer was: ",FText display]}
  where compare' i k = case (Map.lookup (show i) rsp) of
                        (Just input) -> if input == "0" && k == True then True
                                        else if input == "1" && k == False then True
                                        else False
                        _ -> error "Expression not found"
        display = foldl (\a b -> a ++ " " ++ show b) "" sol