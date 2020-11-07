module CS30.Exercises.LogicRewriting.Exercise where

import           CS30.Data
import           CS30.Exercises.Data
import qualified Data.Map as Map
import           CS30.Exercises.LogicRewriting.Parsing (laws, Expr (..))
import           CS30.Exercises.LogicRewriting.ProofGeneration (getDerivation, Proof (..))

-- final exercise type
logicRewritingEx :: ExerciseType
logicRewritingEx = exerciseType "Logic Rewriting" "L?.?" "Logic Rewriting"
                       logicExercises
                       logicQuer
                       logicFeedback

-- display an Expr as a string (to be used with FMath)
-- TODO: display parentheses better
displayExpr :: Expr -> String
displayExpr (Var v)         = [v]
displayExpr (Const b)       = if b then "\\text{true}" else "\\text{false}"
displayExpr (Neg e)         = "\\neg\\left("++(displayExpr e)++"\\right)"
displayExpr (And e1 e2)     = "\\left("++(displayExpr e1)++"\\ \\wedge\\ "++(displayExpr e2)++"\\right)"
displayExpr (Or e1 e2)      = "\\left("++(displayExpr e1)++"\\ \\vee\\ "++(displayExpr e2)++"\\right)"
displayExpr (Implies e1 e2) = "\\left("++(displayExpr e1)++"\\ \\Rightarrow\\ "++(displayExpr e2)++"\\right)"

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
logicExercises = [((getDerivation laws) <$> randomExpr) >>= showExer]
                 where displayStepsExcept _ []   = []
                       displayStepsExcept n (step:steps) = [FMath "\\equiv", name, 
                                                            FIndented 1 [FMath $ displayExpr (snd step)]] 
                                                           ++ displayStepsExcept (n-1) steps
                                                           where correct = FText ("{ "++(fst step)++" }")
                                                                 name = if n/=0 then correct
                                                                        else FChoice "choice" [ [FText "{ DeMorgan's Law }"]
                                                                                              , [FText "{ Idempotent Law }"]
                                                                                              , [correct]
                                                                                              ]
                                                                                              -- ^ TODO: give random choices/order
                       showExer (Proof e steps) = nodes [ ([FIndented 1 [FMath $ displayExpr e]] 
                                                           ++ (displayStepsExcept i steps)
                                                          , "2") -- TODO: change this to randomly decided correct answer 
                                                        | i <- [0..(length steps - 1)]]

-- generate the question displayed to the user
logicQuer :: ([Field], String) -> Exercise -> Exercise
logicQuer fs defExer = defExer {eQuestion = fst fs}

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
