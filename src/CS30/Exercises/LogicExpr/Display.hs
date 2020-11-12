{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.LogicExpr.Display (logicProof) where
import CS30.Exercises.LogicExpr.Parser
import CS30.Exercises.LogicExpr.Proof
import CS30.Data
import CS30.Exercises.Data
import qualified Data.Map as Map
import Debug.Trace

logicProof :: ExerciseType
logicProof
  = exerciseType "RewritingExpressions" "L?.?"
                 "Logic: rewriting expressions" 
                 [(generateRandEx 3), (generateRandEx 5)]
                 generateExercise 
                 generateFeedback

breakUnderscore :: String -> [String]
breakUnderscore s
  =  case dropWhile (=='_') s of
       "" -> []
       s' -> w : breakUnderscore s''
             where (w, s'') = break (=='_') s'

-- Function for generating a random logic expression from p, q, r, ^, ∨, ¬, true, and false.
generateRandEx :: Int -> ChoiceTree (LogicExpr, [Int])
generateRandEx n 
  = do expr <- genRandEx n
       perms <- permutations 15
       return (expr, perms)
    where 
      genRandEx i | i < 1
        = Branch [ Branch [Branch [Node (Var varName), Node (Neg $ Var varName)] | varName <- ['p','q','r']]
                  , Branch [Node (Con True), Node (Con False)]
                ]
      genRandEx i
        = Branch [do { e1 <- genRandEx i'
                      ;e2 <- genRandEx (i - i' - 1)
                      ;opr <- nodes [And,Or,Imply]
                      ;return (if n == i then (Neg $ Bin opr e1 e2) else (Bin opr e1 e2))
                    }
                  | i' <- [0..i-1]
                ]
      permutations 0 = Node []
      permutations l
        = do {i <- nodes [0..l-1]
              ;rm <- permutations (l-1)
              ;return (i : map (\v -> if v >= i then v+1 else v) rm)
            }

-- Function for the actual text displayed to the client.
generateExercise :: (LogicExpr, [Int]) -> Exercise -> Exercise
generateExercise (expr, perm) exercise 
  = exercise{eQuestion = [ FText $"Here is an example proof, can you put it in the right order?"
                         , FIndented 1 [FMath $ show $ initExpr proof] 
                         , FReorder "proof" (map showStep (reorderedSteps proof))]
      , eBroughtBy = ["Tyler", "Fei"] }
  where proof = getDerivation (map parseLaw input_laws) expr
        initExpr (Proof e _) = e
        steps (Proof _ s)  = s
        reorderedSteps p = (map ((steps p) !!) (filter (< (length $ steps p)) perm))
        showStep (name, ex) = [ FMath "=", FText name, FIndented 1 [FMath $ show ex] ]

-- Function for generating feedback and displaying it to the user. 
generateFeedback :: (LogicExpr, [Int]) -> Map.Map String String -> ProblemResponse -> ProblemResponse
generateFeedback (_, sol) rsp pr 
  = case (Map.lookup "proof" rsp) of
      (Just userOrder) 
        -> if map (((filteredSol userOrder) !!) . read) (breakUnderscore userOrder) == [0..((length $ filteredSol userOrder) - 1)]
           then trace (show (filteredSol userOrder)) $ markCorrect pr{prFeedback=[ FText "The order is correct. Your answer was: ",FText (display userOrder)]}
           else trace (show (filteredSol userOrder)) $ markWrong pr{prFeedback=[ FText "The order is incorrect. Your answer was: ",FText (display userOrder)]}
      _ -> error "Response is missing 'proof' field"
  where filteredSol arr = filter (< (length $ breakUnderscore arr)) sol
        display x = map (\c -> if c=='_' then ','; else c) x