{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.LogicExpr.Display (logicProof, genRandEx) where
import CS30.Exercises.LogicExpr.Parser
import CS30.Exercises.LogicExpr.Proof
import CS30.Data
import CS30.Exercises.Data
import qualified Data.Map as Map

logicProof :: ExerciseType
logicProof
  = exerciseType "RewritingExpressions" "L?.?"
                 "Logic: rewriting expressions" 
                 [generateRandEx 3, generateRandEx 5]
                 generateExercise 
                 generateFeedback

breakUnderscore :: String -> [String]
breakUnderscore s
  =  case dropWhile (=='_') s of
       "" -> []
       s' -> w : breakUnderscore s''
             where (w, s'') = break (=='_') s'

-- Function for generating a random logic expression and proof steps from p, q, r, ^, ∨, ¬, true, and false.
generateRandEx :: Int -> ChoiceTree (LogicExpr, [[Field]], [Int])
generateRandEx n 
  = do e <- genRandEx n
       let expr = Neg e
       let proof = getDerivation (map parseLaw input_laws) expr
       perms <- permutations (getSize proof)
       return (expr, shuffle proof perms, perms)

-- Helper function for generateRandEx, generates a ChoiceTree of logic expressions of a certain size.
genRandEx :: Int -> ChoiceTree LogicExpr
genRandEx i | i < 1
  = Branch [ Branch [Branch [Node (Var varName), Node (Neg $ Var varName)] | varName <- ['p','q','r']]
            , Branch [Node (Con True), Node (Con False)]
            ]
genRandEx i
  = Branch [do { e1 <- genRandEx i'
                ;e2 <- genRandEx (i - i' - 1)
                ;opr <- nodes [And,Or,Imply]
                ;return (Bin opr e1 e2)
              }
            | i' <- [0..i-1]
          ]

permutations :: Int -> ChoiceTree [Int]
permutations 0 = Node []
permutations l
  = do {i <- nodes [0..l-1]
        ;rm <- permutations (l-1)
        ;return (i : map (\v -> if v >= i then v+1 else v) rm)
      }

getSize :: Proof a -> Int
getSize (Proof _ steps) = length steps

shuffle :: Proof String -> [Int] -> [[Field]]
shuffle proof perm = map showStep (reorderedSteps proof)
  where steps (Proof _ s)  = s
        reorderedSteps p = map (steps p !!) perm
        showStep (name, ex) = [ FMath "=", FText name, FIndented 1 [FMath $ show ex] ]

-- Function for the actual text displayed to the client.
generateExercise :: (LogicExpr, [[Field]], [Int]) -> Exercise -> Exercise
generateExercise (expr, steps, _) exercise 
  = exercise{eQuestion = [ FText "Here is an example proof, can you put it in the right order?"
                         , FIndented 1 [FMath $ show expr] 
                         , FReorder "proof" steps]
      , eBroughtBy = ["Tyler", "Fei"] }

-- Function for generating feedback and displaying it to the user. 
generateFeedback :: (LogicExpr, [[Field]], [Int]) -> Map.Map String String -> ProblemResponse -> ProblemResponse
generateFeedback (_, _, sol) rsp pr 
  = case Map.lookup "proof" rsp of
      (Just userOrder) 
        -> if map ((sol !!) . read) (breakUnderscore userOrder) == [0..(length sol - 1)]
           then markCorrect pr{prFeedback=[ FText "The order is correct. Your answer was: ",FText (display userOrder)]}
           else markWrong pr{prFeedback=[ FText "The order is incorrect. The correct order was: ",FText displayCorrect]}
      _ -> error "Response is missing 'proof' field"
  where display x = map (\c -> if c=='_' then ','; else c) x
        displayCorrect = foldl (\a b -> a ++ " " ++ show b) "" sol
