module CS30.Exercises.SetCardinalitiesProofs.CardinalityExercises(cardinalityProofExer) where
import CS30.Data
import CS30.Exercises.Data
import CS30.Exercises.SetCardinalitiesProofs.CardinalityProof
import CS30.Exercises.SetCardinalitiesProofs.RuleParser
import qualified Data.Map as Map



-- setExercises :: [ChoiceTree ([Field], Integer)] -- first thing in field will be expression, rest its equivalencies
-- setExercises = [fullExercise 3, fullExercise 5, fullExercise 7]
--   where fullExercise i = do ex <- generateRandSetExpr i
--                             let Proof lhs steps = genProof laws ex
--                                 (_,rhs) = last steps
--                             asgn <- assignRanVal rhs 
--                             let answer = head (evaluate asgn rhs)
--                             let exprs = nubSort (getExprs rhs)
--                             if denominator answer == 1 then return () else error "Resulting answer in the generated puzzle is a fraction, that shouldn't have happened! (Error stems from Sebastiaan's code)" 
--                             return (genFields lhs (evaluate asgn) exprs, numerator answer)
--         showFrac ans | denominator ans == 1 = show (numerator ans)
--         showFrac _ = error "Got a fraction in the generated puzzle, that shouldn't have happened! (Error stems from Sebastiaan's code)"
--         genFields lhs getVal exprs
--           = [FText "Given that "] 
--             ++ combine [FMath (asLatex e ++ "=" ++ showFrac ans) | e <- exprs, ans <- getVal e]
--             ++ [FText ". Compute ", FMath (asLatex lhs)] 


generateRandSetExpr :: Int -> ChoiceTree Expr
generateRandSetExpr n 
    | n < 2 = Branch [ Node (Var varName) | varName <- ['A' .. 'Z']]
generateRandSetExpr n = do {
                            symb <- nodes [Intersection, Union, Powerset, Cartesian, Setminus];
                            if symb == Powerset then
                                do {
                                    expr <- generateRandSetExpr (n-1);
                                    return (Op symb [expr])
                                }
                            else 
                                do {
                                    n' <- nodes [1 .. n-1];
                                    expr1 <- generateRandSetExpr n';
                                    expr2 <- generateRandSetExpr (n - n' - 2);
                                    return (Op symb [expr1, expr2])
                                }
} 

generateQuestion :: Expr -> Exercise -> Exercise
generateQuestion expr exer = undefined

generateFeedback :: Expr -> Map.Map String String -> ProblemResponse -> ProblemResponse
generateFeedback _ _ rsp = rsp

cardinalityProofExer :: ExerciseType
cardinalityProofExer = exerciseType "Cardinality" "L?.?" "Sets: Cardinalities"
                            [generateRandSetExpr 5]
                            generateQuestion
                            generateFeedback

type PossibleVals = [(Expr, Integer)] -- Stores (expression, possible values) for each expression found in the equation

genAllPossibleValues :: Expr -> ChoiceTree PossibleVals
genAllPossibleValues expr = assignAll toAssign
    where
        toAssign = getExprs expr
        assignAll [] = Node []
        assignAll (x:xs) = do {
            possibleVal <- nodes [2 .. 20];
            xs' <- assignAll xs;
            return ((x, possibleVal):xs')
        }


-- evaluate will assign values from genAllpossiblevalues to the cardinaliteis in the rhs of the expression
-- then it will compute

--minus/plus/mult
evaluate :: PossibleVals -> Expr -> Integer
evaluate _ (Var _) = error "Cannot evaluate var"
evaluate pV (Op Cardinality [expr]) = case (lookup expr pV) of
                                    Just i -> i
                                    Nothing -> error ("Content of cardinality needs to be in possible Values. expr in |expr| = " ++ (show expr))
evaluate pV (Op Mult [e1,e2]) = (evaluate pV e1) * (evaluate pV e2)
evaluate pV (Op Add [e1,e2]) = (evaluate pV e1) + (evaluate pV e2)
evaluate pV (Op Sub [e1,e2]) = (evaluate pV e1) - (evaluate pV e2)
                            


getExprs :: Expr -> [Expr]
getExprs (Op Cardinality [e]) = [e]
getExprs (Op _ exprs) = concatMap getExprs exprs
getExprs (Var _) = error "Question is poorly phrased, a set is not a valid variable" -- If we have a set on its own in the expression, we throw an error
