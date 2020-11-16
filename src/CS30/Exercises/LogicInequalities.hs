{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
module CS30.Exercises.LogicInequalities (logicInequalitiesEx) where
import           CS30.Data
import           CS30.Exercises.Data
import qualified Data.Map as Map
import           CS30.Exercises.LogicInequalities.IneqParser
import           CS30.Exercises.LogicInequalities.IneqProofs

{- LogicInequalities.hs -}

{- 
This module generates problems that deal with inequalites, and presents the user with a proof to reorder. 
Using laws in lawList and reasoning for inequalities, the proof simplifies expressions as much as possible before calculating.
The user is asked to reorder the proof and is given feedback on the correct order of the proof.
-}

logicInequalitiesEx :: ExerciseType
logicInequalitiesEx = exerciseType "ineqProof" "L?.???" "Logic: Inequality"
                      [getFields] genProof simpleFeedback
  where simpleFeedback (_,sol) rsp pr
                = case Map.lookup "proof" rsp of
                        Just str
                         -> if map (sol !!) (breakUnderscore str) == [0..length sol - 1]
                            then markCorrect $ pr{prFeedback=[ FIndented 0 [FText "Great work!"]], prTimeToRead = 60}
                            else markWrong $ pr{prFeedback=[ FIndented 0 [ FText "The answer was: "
                                                                         , FText $ show sol]
                                                           , FIndented 0 [ FText "You got the following steps wrong: "
                                                                         , FText $ show $ isRight 0 (map (sol !!) (breakUnderscore str))]
                                                           ], prTimeToRead = 60}
                        Nothing -> error "Client response is missing 'proof' field"

isRight :: Int -> [Int] -> [Int]
isRight _ [] = []
isRight n (x:xs) = [n | n /= x] ++ isRight (n+1) xs

genProof :: ([Field],a) -> Exercise -> Exercise
genProof (fields,_) def 
 = def{ eQuestion = fields
      , eBroughtBy = ["Kyle Bensink and Lucas Boebel"] }

getFields :: ChoiceTree ([Field],[Int])
getFields
  = do e1 <- generateRandEx 4 "n"
       e2 <- generateRandEx 4 "n"
       let proof = makeProof e1 e2 12
       p <- permutations (getSize proof)
       return (shuffle proof p , p)

permutations :: Int -> ChoiceTree [Int]
permutations 0 = Node []
permutations n
 = do i <- nodes [0..n-1]
      rm <- permutations (n-1)
      return (i : map (\v -> if v >= i then v+1 else v) rm)

breakUnderscore :: String -> [Int]
breakUnderscore s
  =  case dropWhile (=='_') s of
       "" -> []
       s' -> (read w) : breakUnderscore s''
             where (w, s'') = break (=='_') s'

shuffle :: Proof -> [Int] -> [Field]
shuffle (Proof start a proofSteps) order = [ FText $"Can you put the following proof in the right order?"
                                            , FIndented 1 [FText "Assuming ", FMath $ "n \\geq " ++ show a, FText " , show ", FMath $ showHandler start]
                                        --     , FIndented 1 [FText "Assuming ", FMath $ "n \\geq " ++ show a]
                                            , FReorder "proof" steps
                                            ]
  where
    steps = map (otherSteps !!) order
    otherSteps = map makeStep proofSteps
    makeStep (Single (one,two))   = [ FText $ "{ " ++ show one ++ " }"
                                    , FIndented 1 [FMath $ show two] ]
    makeStep (Double (s,(e1,i,e2))) = [ FText $ "{ " ++ show s ++ " }"
                                    , FIndented 1 [FMath $ show e1 ++ show i ++ show e2] ]

getSize :: Proof -> Int
getSize (Proof _ _ b) = length b


-- return valid expressions, i.e. those that have a variable in them
-- just calling generateRandEx i < 1 may or may not generate a valid expression
-- but calling generateRandEx i ≥ 1 will filter out the invalid ones
generateRandEx :: Int -> String -> ChoiceTree Expr
generateRandEx i n | i < 1
 = Branch [ Branch [Node (Var varName) | varName <- [n]] -- add support for more variables
          , Branch [Node (Const val) | val <- [2..9]]
          ]
generateRandEx i n
 = filterTree $ Branch [do {e1 <- generateRandEx i' n
                       ;e2 <- generateRandEx (i - i' - 1) n
                       ;opr <- nodes $ availOps i
                       ;return (if opr == Factorial then Op opr [e1] else Op opr [e1,e2])
                       }
                       | i' <- [0..i-1]
                       ]
    where
      availOps j = case (j < 2) of
                    True  -> [Addition,Subtraction,Multiplication,Exponentiation,Factorial]
                    False -> [Addition,Multiplication]

filterTree :: ChoiceTree Expr -> ChoiceTree Expr
filterTree (Branch b@((Branch _):_)) = Branch $ filter isInteresting $ filter isNonEmpty $ map filterTree b
filterTree (Branch n@((Node _):_)) =  Branch $ filter nodeHasVar n
filterTree n = n

-- make sure the generated expression can be modified by at least one of our laws
isInteresting :: ChoiceTree Expr -> Bool
isInteresting (Node n) = length (getProofSteps (getProof n)) > 0
  where
    getProof n' = getSingleDerivation lawBois GThan n' 2
    getProofSteps (OldProof _ ps) = ps
isInteresting (Branch []) = False
isInteresting (Branch b) = or $ map isInteresting b

isNonEmpty :: ChoiceTree Expr -> Bool
isNonEmpty (Node _) = True
isNonEmpty (Branch []) = False
isNonEmpty (Branch b) = or $ map isNonEmpty b

nodeHasVar :: ChoiceTree Expr -> Bool
nodeHasVar (Branch _) = False
nodeHasVar (Node n) = hasVar n
  where
    hasVar (Var _) = True
    hasVar (Const _) = False
    hasVar (Op _ [e1]) = hasVar e1
    hasVar (Op _ [e1,e2]) = hasVar e1 || hasVar e2
    hasVar (Op _ _) = False