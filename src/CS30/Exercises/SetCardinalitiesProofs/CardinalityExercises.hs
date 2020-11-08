module CS30.Exercises.SetCardinalitiesProofs.CardinalityExercises(cardinalityProofExer) where
import           CS30.Data
import           CS30.Exercises.Data
import           CS30.Exercises.SetCardinalitiesProofs.CardinalityProof
import           CS30.Exercises.SetCardinalitiesProofs.RuleParser
import qualified Data.Map as Map

generateRandExpr :: Int -> ChoiceTree Expr
generateRandExpr n | n < 1 
    = Branch [ Node (Var varName) | varName <- ['A' .. 'Z']]
generateRandExpr n 
    = Branch [do {expr1 <- generateRandExpr n';
                  expr2 <- generateRandExpr (n - n' - 1);
                  symb <- nodes [Add, Sub, Mult, Intersection, Union, Powerset, Cardinality, Cartesian, Expon, Setminus];
                  return (Op symb [expr1, expr2])
                 } 
             | n' <- [0 .. n-1]
             ] 

generateQuestion :: Expr -> Exercise -> Exercise
generateQuestion expr exer = undefined

generateFeedback :: Expr -> Map.Map String String -> ProblemResponse -> ProblemResponse
generateFeedback _ _ rsp = rsp

cardinalityProofExer :: ExerciseType
cardinalityProofExer = exerciseType "Cardinality" "L?.?" "Sets: Cardinalities"
                            [generateRandExpr 5]
                            generateQuestion
                            generateFeedback