{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
module CS30.Exercises.LogicInequalities (logicInequalitiesEx) where
import CS30.Data
import CS30.Exercises.Data
import qualified Data.Map as Map
import CS30.Exercises.LogicInequalities.IneqParser

logicInequalitiesEx :: ExerciseType
logicInequalitiesEx = exerciseType "ineqProof" "L?.???" "Logic: Inequality"
                      [permutations 5] genProof simpleFeedback
  where simpleFeedback sol rsp pr
                = case Map.lookup "proof" rsp of
                        Just str
                         -> if map ((sol !!) . read ) (breakUnderscore str) == [0..4]
                            then markCorrect pr
                            else markWrong pr{prFeedback=[FText "You answered: ",FText str]}
                        Nothing -> error "Client response is missing 'proof' field"

permutations :: Int -> ChoiceTree [Int]
permutations 0 = Node []
permutations n -- ChoiceTree is a Monad now! I've also derived "Show", so you can more easily check this out in GHCI.
 = do i <- nodes [0..n-1]
      rm <- permutations (n-1)
      return (i : map (\v -> if v >= i then v+1 else v) rm)

breakUnderscore :: String -> [String]
breakUnderscore s
  =  case dropWhile (=='_') s of
       "" -> []
       s' -> w : breakUnderscore s''
             where (w, s'') = break (=='_') s'

-- data Proof     = Proof Expr [ProofStep] deriving Show
-- type ProofStep = (String, Expr)

genProof :: [Int] -> Exercise -> Exercise
genProof order def 
 = def{ eQuestion = [ FText $"Can you put the following proof in the right order?"
                    , FIndented 1 [FMath "(n + 1)! > n"]
                    , FReorder "proof"
                        (map ([step1,step2,step3,step4,step5] !!) order)
                    ]
      , eBroughtBy = ["Kyle Bensink and Lucas Boebel"] }

      where --hardcoded example
        step1 = [ FMath "=", FText "{ definition of factorial }"
                , FIndented 1 [FMath "(n + 1)! ≥ (n + 1) \\cdot (n)!"] ]
        step2 = [ FMath "=", FText "{ Given x > 0: n + 1 > 0, n! > n }"
                , FIndented 1 [FMath "(n + 1)! > (n + 1) \\cdot (n)!"] ]
        step3 = [ FMath "=", FText "{ n > 0, n + 1 > 1 }"
                , FIndented 1 [FMath "(n + 1)! > (n + 1) \\cdot n"] ]
        step4 = [ FMath "=", FText "{ n > 0, n + 1 > 1 }"
                , FIndented 1 [FMath "(n + 1)! > 1 * n"] ]
        step5 = [ FMath "=", FText "{ Multiplication by 1 }"
                , FIndented 1 [FMath "(n + 1)! > n"] ]

-- return valid expressions, i.e. those that have a variable in them
-- just calling generateRandEx i<1 may or may not generate a valid expression
-- but calling generateRandEx i≥1 will filter out the invalid ones
generateRandEx :: Int -> ChoiceTree Expr
generateRandEx i | i < 1
 = Branch [ Branch [Node (Var varName) | varName <- ["n"]] -- add support for more variables
          , Branch [Node (Const val) | val <- [2..10]]
          ]
generateRandEx i
 = filterTree $ Branch [do {e1 <- generateRandEx i'
                       ;e2 <- generateRandEx (i - i' - 1)
                       ;opr <- nodes [Addition,Subtraction,Multiplication,Division,Exponentiation]
                       ;return (Op opr [e1,e2])
                       }
                       | i' <- [0..i-1]
                       ]

filterTree :: ChoiceTree Expr -> ChoiceTree Expr
filterTree (Branch b@((Branch _):_)) = Branch $ filter isNonEmpty $ map filterTree b
filterTree (Branch n@((Node _):_)) =  Branch $ filter nodeHasVar n
filterTree n = n

isNonEmpty :: ChoiceTree Expr -> Bool
isNonEmpty (Node _) = True
isNonEmpty (Branch []) = False
isNonEmpty (Branch b) = or $ map isNonEmpty b

nodeHasVar :: ChoiceTree Expr -> Bool
nodeHasVar (Branch b) = False
nodeHasVar (Node n) = hasVar n
  where
    hasVar (Var _) = True
    hasVar (Const _) = False
    hasVar (Op _ [e1]) = hasVar e1
    hasVar (Op _ [e1,e2]) = hasVar e1 || hasVar e2
    hasVar (Op _ _) = False
