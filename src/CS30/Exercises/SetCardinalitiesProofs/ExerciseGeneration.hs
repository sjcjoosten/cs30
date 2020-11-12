module CS30.Exercises.SetCardinalitiesProofs.ExerciseGeneration where
import CS30.Data
import CS30.Exercises.Data
import Data.List
import Data.Char
import           CS30.Exercises.Util
import CS30.Exercises.SetCardinalitiesProofs.Proof
import CS30.Exercises.SetCardinalitiesProofs.RuleParser
import qualified Data.Map as Map
import GHC.Stack
import Debug.Trace
import Data.List.Extra
import           Data.Char
import qualified Data.Map as Map
import           Data.Void(Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char


setExercises :: [ChoiceTree ([Field], Integer)] -- first thing in field will be expression, rest its equivalencies
setExercises = [fullExercise 3, fullExercise 5, fullExercise 7]
  where fullExercise i = do ex <- generateRandSetExpr i
                            let Proof lhs steps = genProof laws (Op Cardinality [ex])
                                rhs = last (lhs:map snd steps)
                            asgn <- genAllPossibleValues rhs 
                            let answer = evaluate asgn rhs
                            let exprs = (getExprs rhs) -- nubSort (getExprs rhs)
                            trace ("asgn:" ++ show asgn ++ "\nlhs: " ++ show lhs ++ "\nexprs: " ++ show exprs) (return ())
                            return (genFields lhs (evaluate asgn) exprs, answer)
        
        genFields lhs getVal exprs
          = [FText "Given that "] 
            ++ combine [FMath (exprToLatex e ++ "=" ++ show (getVal e)) | e <- exprs]
            ++ [FText ". Compute ", FMath (exprToLatex lhs)]
            ++ [FFieldMath "Answer"]


-- taken from Raechel and Mahas Project
combine :: [Field] -> [Field]
combine [] = []
combine [x] = [x] 
combine [x,y] = [x, FText " and ", y]
combine [x,y,z] = [x, FText ", ", y, FText ", and ", z]
combine (x:xs) = [x, FText ", "] ++ combine xs


-- dont generate intersections
-- if union,  only union
-- intersection in cardinality will necessitate lookup

generateRandSetExpr i = generateRandSetExpr_helper [Union, Powerset, Cartesian, Setminus] i

generateRandSetExpr_helper :: [Symb] -> Int -> ChoiceTree Expr
generateRandSetExpr_helper lstOfOps n 
    | n < 2 = Branch [ Node (Var varName) 
    | varName <- ['A' .. 'F']]
generateRandSetExpr_helper lstOfOps n = do {
                            symb <- nodes lstOfOps; -- [Union, Powerset, Cartesian, Setminus];
                            if symb == Union then
                                do {
                                    n' <- nodes [1 .. n-1];
                                    expr1 <- generateRandSetExpr_helper [Union] n';
                                    expr2 <- generateRandSetExpr_helper [Union] (n - n' - 2);
                                    return (Op symb [expr1, expr2])
                                }
                            else if symb == Powerset then
                                do {
                                    expr <- generateRandSetExpr_helper lstOfOps (n-1);
                                    return (Op symb [expr])
                                }
                            else 
                                do {
                                    n' <- nodes [1 .. n-1];
                                    expr1 <- generateRandSetExpr_helper lstOfOps n';
                                    expr2 <- generateRandSetExpr_helper lstOfOps (n - n' - 2);
                                    return (Op symb [expr1, expr2])
                                }
} 

generateQuestion :: ([Field], Integer) -> Exercise -> Exercise
generateQuestion (myProblem,sol) def 
    = def { eQuestion =  myProblem--myProblem --, (FText [(show sol)]) 
          , eBroughtBy = ["Paul Gralla","Joseph Hajjar","Roberto Brito"] }


generateFeedback :: ([Field], Integer) -> Map.Map String String -> ProblemResponse -> ProblemResponse
generateFeedback (question, answer) usrRsp pr 
      = reTime$ case answer' of
                  Nothing -> wrong{prFeedback= rsp++(FText " Your answer was "):rspwa} -- Error when parsing. 
                  Just v -> if (v == fromIntegral answer) 
                              then correct{prFeedback=rsp} -- Correct answer.
                              else wrong{prFeedback=rsp++[FText " Your answer was "]++[FMath (show v), FText "."]} -- Incorrect answer.
      where ans = Map.lookup "Answer" usrRsp -- Raw user input. 
            answer' :: Maybe Int
            answer' = case ans of
                  Nothing -> Nothing
                  Just v -> case parse parseAnswer "" v of -- Parses the user input to look for an integer. 
                        Left _ -> Nothing
                        Right st -> Just st
            wrong = markWrong pr
            correct = markCorrect pr
            rsp :: [Field]
            rsp = [FText $ "The cardinality of ", head question, FText " is ", FMath (show (answer)), FText "."] -- Displays the correct answer.
            rspwa = case ans of -- Displays the raw user input. 
                Nothing -> [FText "??? (perhaps report this as a bug?)"]
                Just v -> [FMath v, FText "."]


-- From IncExcCardinalities
parseAnswer :: Parser Int
parseAnswer = unspace digits
  where 
      digits = do ds <- some digit
                  return (foldl1 shiftl ds)
      shiftl m n = 10*m+n
      digit = cvt <$> satisfy isDigit
      cvt d = fromEnum d - fromEnum '0'


cardinalityProofExer :: ExerciseType
cardinalityProofExer = exerciseType "Set Cardinality" "L?.?" "Sets: Cardinalities"
                            setExercises
                            generateQuestion
                            generateFeedback

type PossibleVals = [(Expr, Integer)] -- Stores (expression, possible values) for each expression found in the equation

genAllPossibleValues :: Expr -> ChoiceTree PossibleVals
genAllPossibleValues expr = assignAll toAssign
    where
        toAssign = nubSort (getExprs expr)
        assignAll [] = Node []
        assignAll (x:xs) = do {
            possibleVal <- nodes [2 .. 20];
            xs' <- assignAll xs;
            return ((x, possibleVal):xs')
        }


-- evaluate will assign values from genAllpossiblevalues to the cardinaliteis in the rhs of the expression
-- then it will compute

--minus/plus/mult
evaluate :: HasCallStack => PossibleVals -> Expr -> Integer
evaluate _ (Var _) = error "Cannot evaluate var"
evaluate _ (Val v) = v
evaluate pV expr@(Op Cardinality [_])
                                = case (lookup expr pV) of
                                    Just i -> i
                                    Nothing -> error ("Content of cardinality needs to be in possible Values. expr in |expr| = " ++ (show expr))
evaluate pV (Op Mult [e1,e2]) = (evaluate pV e1) * (evaluate pV e2)
evaluate pV (Op Add [e1,e2]) = (evaluate pV e1) + (evaluate pV e2)
evaluate pV (Op Sub [e1,e2]) = (evaluate pV e1) - (evaluate pV e2)
evaluate _ expr = error ("Cannot evaluate expression: " ++ exprToLatex expr)
                            

getExprs :: Expr -> [Expr]
getExprs e@(Op Cardinality [_]) = [e]
getExprs (Op _ exprs) = concatMap getExprs exprs
getExprs (Var _) = error "Question is poorly phrased, a set is not a valid variable" -- If we have a set on its own in the expression, we throw an error
