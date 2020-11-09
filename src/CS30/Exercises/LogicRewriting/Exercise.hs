module CS30.Exercises.LogicRewriting.Exercise where

import           CS30.Data
import           CS30.Exercises.Data
import           CS30.Exercises.Util
import qualified Data.Map as Map
import           Data.List
import           CS30.Exercises.LogicRewriting.Parsing (laws, lawNames, Expr (..))
import           CS30.Exercises.LogicRewriting.ProofGeneration (getDerivation, Proof (..))
import           Debug.Trace

-- final exercise type
logicRewritingEx :: ExerciseType
logicRewritingEx = exerciseType "LogicRewriting" "L?.?" "Logic Rewriting"
                       logicExercises
                       logicQuer
                       logicFeedback

-- generate initial expressions to put through the proover
-- TODO: decide whether there is a better way to generate "good" expressions
--   (e.g. do we want to have constants in the expression at all? 
--    can we guarantee that at least one variable is in the expression?)
randomExpr :: ChoiceTree Expr
randomExpr = Neg <$> exprOfSize 5
    where exprOfSize :: Int -> ChoiceTree Expr
          exprOfSize 1 = Branch [nodes (map Const [True,False]), 
                                 nodes (map Var ['p','q','r'])]
          exprOfSize 2 = Branch [Neg <$> exprOfSize 1]
          exprOfSize n = Branch ([And <$> exprOfSize i <*> exprOfSize (n-i-1)
                                | i <- [1..(n-2)]] ++ 
                                [Or <$> exprOfSize i <*> exprOfSize (n-i-1)
                                | i <- [1..(n-2)]] ++ 
                                [Implies <$> exprOfSize i <*> exprOfSize (n-i-1)
                                | i <- [1..(n-2)]])

-- contains all the exercises: the list of Fields is what we display
-- and the String is the solution (actually just the index of the right choice)
logicExercises :: [ChoiceTree ([Field], String)]
logicExercises = [do e <- randomExpr
                     let (Proof e' steps) = getDerivation laws e
                     remStep <- nodes [0..(length steps - 1)]
                     let (stepName, stepE) = steps!!remStep
                     choices <- (getOrderedSubset (delete stepName lawNames) 2)
                     correctN <- nodes [0..2]
                     let shuffChoices = putElemIn stepName correctN choices 
                     return (showExer (Proof e' steps) remStep shuffChoices, show correctN)
                  ]
                 where putElemIn :: a -> Int -> [a] -> [a]
                       putElemIn y 0 xs = y:xs
                       putElemIn y n (x:xs) = x:(putElemIn y (n-1) xs)
                       displayStepsExcept _ [] _  = []
                       displayStepsExcept n (s:rem) choices = [FMath "\\equiv", name, 
                                                               FIndented 1 [FMath $ show (snd s)]]
                                                              ++ displayStepsExcept (n-1) rem choices
                                                              where correct = FText ("{ "++(fst s)++" }")
                                                                    name = if n/=0 then correct
                                                                           else FChoice "choice" (map (\x -> [FText $ "{ "++x++" }"]) choices)

                       showExer (Proof e steps) remStep choices = [FIndented 1 [FMath $ show e]] 
                                                                  ++ (displayStepsExcept remStep steps choices)

-- generate the question displayed to the user
logicQuer :: ([Field], String) -> Exercise -> Exercise
logicQuer (quer, _) defExer = defExer {eQuestion = quer,eBroughtBy=["Chibuzo","Lucas"]}

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
