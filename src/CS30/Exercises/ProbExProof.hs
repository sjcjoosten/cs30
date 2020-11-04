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

probExProof :: ExerciseType
probExProof = exerciseType "ProbExProof" "L?.?" "Probability : Expected Value"
            probExercises --choiceTreeList
            genQuestion
            undefined -- genFeedback

probExercises :: [ChoiceTree ([Field], Integer)] -- first thing in field will be expression, rest its equivalencies
probExercises = [fullExercise 3, fullExercise 5, fullExercise 7]
  where fullExercise i = do ex <- genRanEx i
                            let Proof lhs steps = genProof laws ex
                                (_,rhs) = last steps
                            (exEval, covEval) <- assignRanVal rhs 
                            let answer = evaluate exEval covEval rhs

                            return (genFields lhs exEval covEval, answer)
        genFields lhs exEval covEval 
          = [FText "Given that "] 
            ++ combine ([FMath ("E[" ++ s ++ "]=" ++ show i) | (s,i) <- exEval] 
            ++ [FMath ("\\text{cov}(" ++ s1 ++ "," ++ s2 ++ ")=" ++ show i) | (s1,s2,i) <- covEval])

            ++ [FText ". Compute ", FMath (asLaTeX lhs)] 

combine :: [Field] -> [Field]
combine [] = []
combine [x] = [x] 
combine [x,y] = [x, FText " and ", y]
combine [x,y,z] = [x, FText ", ", y, FText ", and ", z]
combine (x:xs) = [x, FText ", "] ++ combine xs

-- take a function, generate latex string
asLaTeX :: ExExpr -> String 
asLaTeX = undefined



    

-- randomSelect (Branch choiceTreeList)
genQuestion :: ([Field], a) -> Exercise -> Exercise
genQuestion (question, _) ex = ex{eQuestion = question ++ [FFieldMath "answer"]}

genProof :: [Law] -> ExExpr -> Proof
genProof = undefined

-- takes inputs to evaluate expected value
type ExValEval = [(String, Integer)]
-- takes inputs to evaluate covariance
type CoVarEval = [(String, String, Integer)]

evaluate :: ExValEval -> CoVarEval -> ExExpr -> Integer
evaluate = undefined

assignRanVal :: ExExpr -> ChoiceTree (ExValEval, CoVarEval)
assignRanVal = undefined
