module CS30.Exercises.ProbExProof where
import CS30.Data
import CS30.Exercises.Data
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Control.Monad.Combinators.Expr
import           Data.Functor.Identity


data ExExpr =
            Econst Integer
            | Eranvar String
            | EbinOp Eop ExExpr ExExpr 
            | Ecov ExExpr ExExpr
            | E ExExpr 

data Eop = Plus | Minus | Times

type Parser = ParsecT Void String Identity

type Equation = (ExExpr, ExExpr)

data Law = Law String Equation 

data Proof = Proof ExExpr [Step]
type Step = (String, ExExpr)

genRanEx :: Int -> ChoiceTree ExExpr
genRanEx = undefined

parseExExpr :: Parser ExExpr
parseExExpr = undefined

parseLawExExpr :: Parser Law
parseLawExExpr = undefined

probaEx :: ExerciseType
probaEx = exerciseType "ProbExProof" "L?.?" "Probability : Expected Value"
            undefined --choiceTreeList
            genQuestion
            undefined -- genFeedback

-- randomSelect (Branch choiceTreeList)
genQuestion :: ([Field], Rational) -> Exercise -> Exercise
genQuestion (question, _) ex = ex{eQuestion = question ++ [FFieldMath "answer"]}

genProof :: [Law] -> ExExpr -> Proof
genProof = undefined

-- takes inputs to evaluate expected value
type ExValEval = (String -> Integer)
-- takes inputs to evaluate covariance
type CoVarEval = (String -> String -> Integer)

evaluate :: ExValEval -> CoVarEval -> ExExpr -> Integer
evaluate = undefined

assignRanVal :: ExExpr -> ChoiceTree (ExValEval, CoVarEval)
assignRanVal = undefined
