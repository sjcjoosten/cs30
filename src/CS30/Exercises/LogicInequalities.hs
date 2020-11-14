{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
module CS30.Exercises.LogicInequalities (logicInequalitiesEx) where
import           CS30.Data
import           CS30.Exercises.Data
import qualified Data.Map as Map
import           CS30.Exercises.LogicInequalities.IneqParser
import           CS30.Exercises.LogicInequalities.IneqProofs
import           Data.Maybe

logicInequalitiesEx :: ExerciseType
logicInequalitiesEx = exerciseType "ineqProof" "L?.???" "Logic: Inequality"
                      [getFields] genProof simpleFeedback
  where simpleFeedback (_,sol) rsp pr
                = case Map.lookup "proof" rsp of
                        Just str
                         -> if map (sol !!) (breakUnderscore str) == [0..length sol - 1]
                            then markCorrect pr
                            else markWrong pr{prFeedback=[ FText "You answered: ", FText $ show (breakUnderscore str)
                                                         , FText " but the answer was: ", FText $ show sol]}
                        Nothing -> error "Client response is missing 'proof' field"


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
                                            , FIndented 1 [FMath $ showHandler start]
                                            , FIndented 1 [FText "Assuming ", FMath $ "n \\geq " ++ show a]
                                            , FReorder "proof" steps
                                            ]
  where
    steps = map (otherSteps !!) order
    otherSteps = map makeStep proofSteps
    makeStep (Single (one,two))   = [ FText $ "{ " ++ show one ++ " }"
                                    , FIndented 1 [FMath $ show two] ]
    makeStep (Double (a,(b,c,d))) = [ FText $ "{ " ++ show a ++ " }"
                                    , FIndented 1 [FMath $ show b ++ show c ++ show d] ]

getSize :: Proof -> Int
getSize (Proof _ _ b) = length b

getFields :: ChoiceTree ([Field],[Int])
getFields
  = do e1 <- generateRandEx 4 "n"
       e2 <- generateRandEx 4 "n"
       let proof = makeProof e1 e2 15
       p <- permutations (getSize proof)
       return (shuffle proof p , p)

genProof :: ([Field],a) -> Exercise -> Exercise
genProof (fields,_) def 
 = def{ eQuestion = fields
      , eBroughtBy = ["Kyle Bensink and Lucas Boebel"] }

generateRandIneq :: ChoiceTree Ineq
generateRandIneq = Branch [Node GThan, Node GEq]

-- return valid expressions, i.e. those that have a variable in them
-- just calling generateRandEx i<1 may or may not generate a valid expression
-- but calling generateRandEx i≥1 will filter out the invalid ones
generateRandEx :: Int -> String -> ChoiceTree Expr
generateRandEx i n | i < 1
 = Branch [ Branch [Node (Var varName) | varName <- [n]] -- add support for more variables
          , Branch [Node (Const val) | val <- [2..9]]
          ]
generateRandEx i n
 = filterTree $ Branch [do {e1 <- generateRandEx i' n
                       ;e2 <- generateRandEx (i - i' - 1) n
                       ;opr <- nodes $ availOps i
                       ;return (Op opr [e1,e2])
                       }
                       | i' <- [0..i-1]
                       ]
    where
      availOps j = case (j < 2) of
                    True -> [Addition,Subtraction,Multiplication,Exponentiation]
                    False -> [Addition,Multiplication]

filterTree :: ChoiceTree Expr -> ChoiceTree Expr
filterTree (Branch b@((Branch _):_)) = Branch $ filter isNonEmpty $ map filterTree b
filterTree (Branch n@((Node _):_)) =  Branch $ filter nodeHasVar n
filterTree n = n

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