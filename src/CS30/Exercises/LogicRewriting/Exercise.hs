module CS30.Exercises.LogicRewriting.Exercise where

import           CS30.Data
import           CS30.Exercises.Data
import           CS30.Exercises.Util
import qualified Data.Map as Map
import           Data.List
import           CS30.Exercises.LogicRewriting.Parsing (laws, lawNames, Expr (..))
import           CS30.Exercises.LogicRewriting.ProofGeneration (getDerivation, Proof (..))

-- final exercise type
logicRewritingEx :: ExerciseType
logicRewritingEx = exerciseType "Logic Rewriting" "L?.?" "Logic Rewriting"
                       logicExercises
                       logicQuer
                       logicFeedback

-- generate initial expressions to put through the prover
-- TODO: decide whether there is a better way to generate "good" expressions
--   (e.g. do we want to have constants in the expression at all? 
--    can we guarantee that at least one variable is in the expression?)
randomExpr :: ChoiceTree Expr
randomExpr = Neg <$> exprOfSize 6

exprOfSize :: Int -> ChoiceTree Expr
exprOfSize 1 = Branch [nodes (map Const [True,False]), 
                        nodes (map Var ['p','q','r'])]
exprOfSize 2 = Branch [Neg <$> exprOfSize 1]
exprOfSize n = Branch ([And <$> exprOfSize i <*> exprOfSize (n-i-1)
                    | i <- [1..(n-2)]] ++ 
                    [Or <$> exprOfSize i <*> exprOfSize (n-i-1)
                    | i <- [1..(n-2)]] ++
                    [Implies <$> exprOfSize i <*> exprOfSize (n-i-1)
                    | i <- [1..(n-2)]])

-- extracts all the variables from an expression
getVars :: Expr -> [Char]
getVars (Var v) = [v]
getVars (Const _) = []
getVars (Neg e) = getVars e
getVars (And e1 e2) = getVars e1 ++ getVars e2
getVars (Or e1 e2) = getVars e1 ++ getVars e2
getVars (Implies e1 e2) = getVars e1 ++ getVars e2

-- assigns random expressions to the variables
-- of constant size 4. TODO: vary the size of the expression generated
assignRandExprs :: [Char] -> [(Char, ChoiceTree Expr)]
assignRandExprs [] = []
assignRandExprs (x:xs) = (x, exprOfSize 4) : assignRandExprs xs

-- substitutes generated expressions for the variables
apply :: [(Char, ChoiceTree Expr)] -> Expr -> ChoiceTree Expr
apply assignments (Var v) = [e' | (v, e') <- assignments] !! 0 
apply _ (Const a) = Node (Const a)
apply assignments (Neg e) = Neg <$> apply assignments e
apply assignments (And e1 e2) = do e1' <- apply assignments e1
                                   e2' <- apply assignments e2
                                   return (And e1' e2')
apply assignments (Or e1 e2) = do e1' <- apply assignments e1
                                  e2' <- apply assignments e2
                                  return (Or e1' e2')
apply assignments (Implies e1 e2) = do e1' <- apply assignments e1
                                       e2' <- apply assignments e2
                                       return (Implies e1' e2')                                                                     

-- generate an expression to fit the law given by the expression
forceLaw :: Expr -> ChoiceTree Expr
forceLaw e = let vars = nub $ getVars e
                 assignments = assignRandExprs vars
             in apply assignments e


-- contains all the exercises: the list of Fields is what we display
-- and the String is the solution (actually just the index of the right choice)
logicExercises :: [ChoiceTree ([Field], String)]
logicExercises = [do e <- randomExpr
                     let (Proof e' steps) = getDerivation laws e
                     remStep <- nodes [0..(length steps - 1)]
                     let (stepName, _) = steps!!remStep
                     choices <- (getOrderedSubset (delete stepName lawNames) 2)
                     correctN <- nodes [0..2]
                     let shuffChoices = putElemIn stepName correctN choices 
                     return (showExer (Proof e' steps) remStep shuffChoices, show correctN)
                  ]
                 where putElemIn :: a -> Int -> [a] -> [a]
                       putElemIn y 0 xs = y:xs
                       putElemIn y _ [] = y:[]
                       putElemIn y n (x:xs) = x:(putElemIn y (n-1) xs)
                       displayStepsExcept _ [] _  = []
                       displayStepsExcept n (s:rems) choices = [FMath "\\equiv", name, 
                                                               FIndented 1 [FMath $ show (snd s)]]
                                                              ++ displayStepsExcept (n-1) rems choices
                                                              where correct = FText ("{ "++(fst s)++" }")
                                                                    name = if n/=0 then correct
                                                                           else FChoice "choice" (map (\x -> [FText $ "{ "++x++" }"]) choices)

                       showExer (Proof e steps) remStep choices = [FIndented 1 [FMath $ show e]] 
                                                                  ++ (displayStepsExcept remStep steps choices)

-- generate the question displayed to the user
logicQuer :: ([Field], String) -> Exercise -> Exercise
logicQuer (quer, _) defExer = defExer {eQuestion = quer}

-- generate feedback
logicFeedback :: ([Field], String) -> Map.Map String String -> ProblemResponse -> ProblemResponse
logicFeedback (_, sol) mStrs defaultRsp 
    = case rsp of 
        Just v -> if v == sol then markCorrect $ defaultRsp{prFeedback = [FText "Correct"]}
                  else if v == "" then markWrong $ defaultRsp{prFeedback = [FText "Please select which law is applied"]}
                       else markWrong $ defaultRsp{prFeedback = [FText "Incorrect"]} 
        Nothing -> markWrong $ defaultRsp{prFeedback = [FText "We could not understand your answer"]}
        -- ^ TODO: give better feedback (probably need to change the solution data structure to show the correct answer)
    where rsp = Map.lookup "choice" mStrs
