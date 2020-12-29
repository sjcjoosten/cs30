{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.LogicExpr.CreativeDisplay (logicWrongStepEx) where
import CS30.Exercises.LogicExpr.Parser
import CS30.Exercises.LogicExpr.Proof
import CS30.Exercises.LogicExpr.Display
import CS30.Data
import CS30.Exercises.Data
import qualified Data.Map as Map

logicWrongStepEx :: ExerciseType
logicWrongStepEx
  = exerciseType "LogicWrongStep" "L3.2"
                 "Logic: rewriting expressions (identify wrong steps)" 
                 [generateRandFaultyEx 2, generateRandFaultyEx 3, generateRandFaultyEx 4]
                 generateExercise 
                 generateFeedback

-- Function for generating a random faulty proof from p, q, r, ^, ∨, ¬, true, and false.
generateRandFaultyEx :: Int -> ChoiceTree (Proof Bool)
generateRandFaultyEx n
  = do e <- genRandEx n
       return $ maxSizeProof n (genFaultyProof (Neg e))

-- variant of map that passes each element's index as a second argument to f
-- https://stackoverflow.com/questions/16191824/index-of-element-in-list-in-haskell
mapInd :: (a -> Int -> b) -> [a] -> [b]
mapInd f l = zipWith f l [0..]

-- Function for the actual text displayed to the client.
generateExercise :: Proof Bool -> Exercise -> Exercise
generateExercise (Proof expr steps) exercise 
  = exercise{ eQuestion = [ FText $"Identify whether each step is correct or incorrect in the proof below."
                          , FTable ( [Cell (FIndented 0 []),Cell (FIndented 1 [FMath $ show expr]),Cell (FIndented 0 [])]
                                   : mapInd showStep steps
                                   )
                          ]
            , eBroughtBy = ["Tyler", "Fei"] }
  where showStep (_, ex) i = [ Cell (FMath "="), Cell (FIndented 1 [FMath $ show ex])
                             , Cell (FChoice (show i) [[FText "Correct"], [FText "Incorrect"]]) ]

-- Function for generating feedback and displaying it to the user. 
generateFeedback :: Proof Bool -> Map.Map String String -> ProblemResponse -> ProblemResponse
generateFeedback (Proof expr steps) rsp pr 
  = if and $ zipWith compare' [0..] steps
    then markCorrect pr{prFeedback=[ FText "Well done."]}
    else markWrong pr{prFeedback=[ FText "Your selections had errors. The correct answers were: "
                                 , FTable ( [Cell (FIndented 0 []),Cell (FIndented 0 [FMath $ show expr]),Cell (FIndented 0 []),Cell (FIndented 0 [])]
                                          : mapInd showStep steps
                                          )]}
  where compare' i (k,_) = case Map.lookup (show (i::Int)) rsp of
                             (Just input) -> (input == "0" && k) || (input == "1" && not k)                                                                                
                             _ -> error "Expression not found"
        showStep (b, ex) i = [ Cell (FMath "=")
                             , Cell (FMath $ show ex)
                             , Cell (FText (showCor b))
                             , Cell (FIndented 0 (case Map.lookup (show i) rsp of
                                                     Just ""      -> [FText "(Left unanswered)"]
                                                     Just "0" | b -> [FText "OK"]
                                                     Just "0"     -> [FText "You answered 'Correct'"]
                                                     Just "1" | not b -> [FText "OK"]
                                                     Just "1"     -> [FText "You answered 'Incorrect'"]
                                                     _            -> []
                                                 ))
                             ]
        showCor True = "Correct"
        showCor False = "Incorrect"