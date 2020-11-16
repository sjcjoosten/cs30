{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.LogicExpr.Display (logicProof) where
import CS30.Exercises.LogicExpr.Parser
import CS30.Exercises.LogicExpr.Proof
import CS30.Data
import CS30.Exercises.Data
import qualified Data.Map as Map

logicProof :: ExerciseType
logicProof
  = exerciseType "RewritingExpressions" "L?.?"
                 "Logic: rewriting expressions" 
                 [(generateRandEx 3)]
                 generateExercise 
                 generateFeedback

-- Function for generating a random logic expression from p, q, r, ^, ∨, ¬, true, and false.
generateRandEx :: Int -> ChoiceTree LogicExpr
generateRandEx n = genRandEx n
  where 
    genRandEx i | i < 1
      = Branch [ Branch [Branch [Node (Var varName), Node (Neg $ Var varName)] | varName <- ['p','q','r']]
                , Branch [Node (Con True), Node (Con False)]
              ]
    genRandEx i
      = Branch [do { e1 <- genRandEx i'
                    ;e2 <- genRandEx (i - i' - 1)
                    ;opr <- nodes [And,Or,Imply]
                    ;return (if n == i then Neg $ Bin opr e1 e2 else Bin opr e1 e2)
                   }
                | i' <- [0..i-1]
              ]

-- Function for the actual text displayed to the client.
generateExercise :: LogicExpr -> Exercise -> Exercise
generateExercise expr exercise 
  = exercise{eQuestion = [ FText $"Here is an example proof, can you put it in the right order?"
                         , FIndented 1 [FMath $ show $ initExpr proof] 
                         , FReorder "proof" (map showStep (steps proof))]
      , eBroughtBy = ["Tyler", "Fei"] }
  where proof = getDerivation (map parseLaw input_laws) expr
        initExpr (Proof e _) = e
        steps (Proof _ s) = s
        showStep (name, ex) = [ FMath "=", FText name, FIndented 1 [FMath $ show ex] ]

generateFeedback :: LogicExpr -> Map.Map String String -> ProblemResponse -> ProblemResponse
generateFeedback _ _ pr = pr