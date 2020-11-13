module CS30.Exercises.LogicRewriting.Exercise where

import           CS30.Data
import           CS30.Exercises.Data
import           CS30.Exercises.Util
import qualified Data.Map as Map
import           Data.List
import           CS30.Exercises.LogicRewriting.Parsing (laws, lawNames, Expr (..), Law (..))
import           CS30.Exercises.LogicRewriting.ProofGeneration (getDerivation, Proof (..), apply)

-- final exercise type
logicRewritingEx :: ExerciseType
logicRewritingEx = exerciseType "Logic Rewriting" "L?.?" "Logic Rewriting"
                       logicExercises
                       logicQuer
                       logicFeedback

-- generate initial expressions to put through the prover
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
assignRandExprs :: [Char] -> ChoiceTree [(Char, Expr)]
assignRandExprs [] = return []
assignRandExprs (x:xs) = do varExpPair <- genPair x
                            remPairs <- assignRandExprs xs
                            return (varExpPair:remPairs)
                         where genPair :: Char -> ChoiceTree (Char, Expr)
                               genPair c = (exprOfSize 4) >>= (\expr -> return (c, expr))                                                              

-- generate an expression to fit the law given by the expression
forceLaw :: Expr -> ChoiceTree Expr
forceLaw e = do let vars = nub $ getVars e
                assignments <- assignRandExprs vars
                return (apply assignments e)

getLawsByName :: String -> ChoiceTree Law
getLawsByName name = nodes [(Law nm eq) | (Law nm eq) <- laws, nm == name]

-- contains all the exercises: the list of Fields is what we display
-- and the String is the solution (actually just the index of the right choice)
logicExercises :: [ChoiceTree ([Field], String)]
logicExercises = [do (Law testName (expr, _)) <- getLawsByName name
                     e <- Neg <$> forceLaw expr
                     let (Proof e' steps) = getDerivation laws e
                     remStep <- nodes $ findIndices ((== testName) . fst) steps
                     let (stepName, _) = steps!!remStep
                     choices <- (getOrderedSubset (delete stepName lawNames) 2)
                     correctN <- nodes [0..2]
                     let shuffChoices = putElemIn stepName correctN choices 
                     return (showExer (Proof e' steps) remStep shuffChoices, show correctN)
                  | name <- lawNames]
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
