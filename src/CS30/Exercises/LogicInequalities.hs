{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE InstanceSigs #-}
module CS30.Exercises.LogicInequalities (logicInequalitiesEx) where
import           CS30.Data
import           CS30.Exercises.Data
import qualified Data.Map as Map
import           CS30.Exercises.LogicInequalities.IneqParser
import           CS30.Exercises.LogicInequalities.IneqProofs

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
shuffle (Proof start proofSteps) order = [ FText $"Can you put the following proof in the right order?"
                                            , FIndented 1 [FMath $ show start ++ show (eqThing proofSteps) ++ show (lastThing proofSteps)]
                                            , FIndented 1 [FText "Assuming ", FMath $ showHandler (middleThing proofSteps)]
                                            , FReorder "proof" steps
                                            ]
  where
    eqThing pSteps = case last pSteps of
                        Single (_,_) -> EqEq
                        Double (_,(_,q,_)) -> q
    lastThing pSteps = case last pSteps of
                        Single (_,_) -> Var "n"
                        Double (_,(_,_,e)) -> e
    middleThing [] = (Var "n", GThan, Const 0)
    middleThing (step:moreSteps) = case step of
                                     Double ("Assumption",ineq) -> ineq
                                     _ -> middleThing moreSteps
    steps = map (otherSteps !!) order
    otherSteps = map makeStep proofSteps
    makeStep (Single (one,two))   = [ FText $ "{ " ++ show one ++ " }"
                                    , FIndented 1 [FMath $ show two] ]
    makeStep (Double (a,(b,c,d))) = [ FText $ "{ " ++ show a ++ " }"
                                    , FIndented 1 [FMath $ show b ++ show c ++ show d] ]

getSize :: Proof -> Int
getSize (Proof _ b) = length b

getFields :: ChoiceTree ([Field],[Int])
getFields
  = do e1 <- generateRandEx 4 "n"
       e2 <- generateRandEx 4 "m"
       ineq <- generateRandIneq
       let proof = getDerivationLengthN lawBois ineq e1 e2 10
       p <- permutations (getSize proof)
       return (shuffle proof p , p)

genProof :: ([Field],a) -> Exercise -> Exercise
genProof (fields,_) def 
 = def{ eQuestion = fields
      , eBroughtBy = ["Kyle Bensink and Lucas Boebel"] }

-- smartForceLaw e = do let vars = nubSort (getVars e)
--                      assignments <- assignRandExprs (siz - sizeOf e) vars
--                      return (apply assignments e)

-- forceLaw :: Int -> (Expr, b) -> ChoiceTree Expr
-- forceLaw n (ENot e) = do e' <- forceLaw (n - 1) e
--                          return (ENot e')
-- forceLaw n (EBinOp o e1 e2) = do s <- nodes [1..(n-1)]
--                                  e1' <- forceLaw (s) e1
--                                  e2' <- forceLaw (n - s - 1) e2
--                                  return (EBinOp o e1' e2')
-- forceLaw n (EVar e) = genRandEx ne1 <- genRandEx1e2 <- genRandEx2let proof = genProof (ENot (EBinOp Op e1 e2))not (expr)