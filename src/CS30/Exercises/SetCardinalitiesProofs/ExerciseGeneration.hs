module CS30.Exercises.SetCardinalitiesProofs.ExerciseGeneration where
import CS30.Data
import CS30.Exercises.Data
import Data.Char
import CS30.Exercises.Util
import CS30.Exercises.SetCardinalitiesProofs.Proof
import CS30.Exercises.SetCardinalitiesProofs.RuleParser
import qualified Data.Map as Map
import GHC.Stack
import Debug.Trace
import Data.List.Extra
import Text.Megaparsec

type SetCardinalityProblem = ([Field], Proof, Integer)
type PossibleVals = [(Expr, Integer)] -- Stores (expression, possible values) for each expression found in the equation


-- TODO:
-- get rid of warnings
-- try to import functions combine and parseAnswer
-- maybe make latex a little prettier
-- change/improve law names
-- general organization

cardinalityProofExer :: ExerciseType
cardinalityProofExer 
    = exerciseType  "Set Cardinality" "L?.?" 
                    "Sets: Cardinalities"
                    setExercises
                    generateQuestion
                    generateFeedback



setExercises :: [ChoiceTree SetCardinalityProblem] -- first thing in field will be expression, rest its equivalencies
setExercises = [fullExercise 3, fullExercise 5, fullExercise 6]
  where fullExercise i = do ex <- generateRandSetExpr i
                            let proof@(Proof lhs steps) = genProof laws (Op Cardinality [ex])
                                rhs = last (lhs:map snd steps)
                            asgn <- genAllPossibleValues rhs 
                            let answer = evaluate asgn rhs
                            let exprs = (getExprs rhs) -- nubSort (getExprs rhs)
                            trace ("asgn:" ++ show asgn ++ "\nlhs: " ++ show lhs ++ "\nexprs: " ++ show exprs) (return ())
                            return (genFields lhs (evaluate asgn) exprs, proof, answer)

        genFields lhs getVal exprs
          = [FText "Given that "] 
            ++ combine [FMath (exprToLatex e ++ "=" ++ show (getVal e)) | e <- (filter (\x -> x /= (Val a) ) 
            exprs)]
            ++ [FText ". Compute ", FMath (exprToLatex lhs)]
            ++ [FFieldMath "Answer"]

generateQuestion :: SetCardinalityProblem -> Exercise -> Exercise
generateQuestion (myProblem, proof, sol) def 
    = def { eQuestion =  myProblem--myProblem --, (FText [(show sol)]) 
          , eBroughtBy = ["Paul Gralla","Joseph Hajjar","Roberto Brito"] }


-- Just noticed a bit of a visual bug. When the answer is correct,
-- a text box appears alongside the answer.
generateFeedback :: SetCardinalityProblem -> Map.Map String String -> ProblemResponse -> ProblemResponse
generateFeedback (question, (Proof proofExpr proofSteps), answer) usrRsp pr 
      = reTime$ case answer' of
                  Nothing -> wrong{prFeedback=(FText " Ill formatted reponse, please only input integers, answer was "):rspwa} -- Error when parsing. 
                  Just v -> if (v == fromIntegral answer) 
                              then correct{prFeedback=correctRsp} -- Correct answer.
                              else wrong{prFeedback=wrongRsp} -- ++[FText " Your answer was "]++[FMath (show v), FText "."]} -- Incorrect answer.
      where ans = Map.lookup "Answer" usrRsp -- Raw user input. 
            answer' :: Maybe Int
            answer' = case ans of
                  Nothing -> Nothing
                  Just v -> case parse parseAnswer "" v of -- Parses the user input to look for an integer. 
                        Left _ -> Nothing
                        Right st -> Just st
            wrong = markWrong pr
            correct = markCorrect pr

            correctRsp = [FText $ "Correct! The cardinality is", FMath (show (answer)), FText "."] -- Displays the correct answer.
            rspwa = case ans of -- Displays the raw user input. 
                Nothing -> [FText "??? (perhaps report this as a bug?)"]
                Just v -> [FMath v, FText "."]

            wrongRsp = [FText ("incorrect response, these are the steps to calculate"), FMath (exprToLatex proofExpr ++ "=" ++ show answer), FText "with the given values",
                        FTable (
                            [headerRow] ++           
                            [[Cell (FText "Expression") , Cell (FMath ("\\quad" ++ exprToLatex proofExpr))]] ++
                            [[Cell (FText rule) , Cell (FMath ("= " ++ exprToLatex step))] | (rule, step) <- proofSteps]                                                                          -- add header row to the table                        
                        )
                    ]
            headerRow = [Header (FText "Rule"), Header (FText "Deduction Step")]

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

generateRandSetExpr :: Int -> ChoiceTree Expr
generateRandSetExpr i = fst . flip setVars ['A' ..] <$> (generateRandSetExpr_helper [Union, Powerset, Cartesian, Setminus] i)
    
setVars :: Expr -> [Char] -> (Expr, [Char])
setVars e@(Val a) varNames = (e, varNames)
setVars (Var _) (possible:possibles) = (Var possible, possibles)
setVars (Op o [expr]) possibles = 
        let (expr', newPossible) = setVars expr possibles in (Op o [expr'], newPossible)
setVars (Op o [expr1,expr2]) possibles = 
        let (expr1', newPossible1) = setVars expr1 possibles
            (expr2', newPossible2) = setVars expr2 newPossible1
        in
            (Op o [expr1',expr2'], newPossible2)

generateRandSetExpr_helper :: [Symb] -> Int -> ChoiceTree Expr
generateRandSetExpr_helper _ n 
    | n < 2 = Branch [ Node (Var varName) | varName <- ['A']]
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
                                    expr <- generateRandSetExpr_helper [Union, Setminus] (n-1);
                                    return (Op symb [expr])
                                }
                            else 
                                do {
                                    n' <- nodes [1 .. n-1];
                                    expr1 <- generateRandSetExpr_helper [Union, Setminus] n';
                                    expr2 <- generateRandSetExpr_helper [Union, Setminus] (n - n' - 2);
                                    return (Op symb [expr1, expr2])                                
                                }
} 




-- From IncExcCardinalities.hs (maybe import)
parseAnswer :: Parser Int
parseAnswer = unspace digits
  where 
      digits = do ds <- some digitP
                  return (foldl1 shiftl ds)
      shiftl m n = 10*m+n
      digitP = cvt <$> satisfy isDigit
      cvt d = fromEnum d - fromEnum '0'


genAllPossibleValues :: Expr -> ChoiceTree PossibleVals
genAllPossibleValues expr = assignAll toAssign
    where
        toAssign = nubSort (getExprs expr)  -- List of all expression (e.g. Var A, Op Intersection [Var A, Var C], Op Intersection [Var B,Var C] ] )
        assignAll [] = Node []
        assignAll (x:xs) = do {
            intersectionPossibleVal <- nodes [2 .. 10];
            varPossibleVal <- nodes [11 .. 20];
            genericPossibleVal <- nodes [2 .. 20];
            xs' <- assignAll xs;
            case (x) of
                Val (x')                            -> return ((x, x'):xs'); -- Only possible value for a constant is itself
                Op Cardinality [(Var _)]            -> return ((x, varPossibleVal):xs');
                Op Cardinality [(Op Intersection _)]-> return ((x, intersectionPossibleVal):xs');
                Op Cardinality [(Op Setminus _)]    -> return ((x, intersectionPossibleVal):xs');
                _                                   -> return ((x, genericPossibleVal):xs');
        }


-- evaluate will assign values from genAllpossiblevalues to the cardinalities in the rhs of the expression
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
evaluate pV (Op Expon [(Val i), e]) = i^(evaluate pV e)
evaluate _ expr = error ("Cannot evaluate expression: " ++ exprToLatex expr)
                            

getExprs :: Expr -> [Expr]
getExprs e@(Op Cardinality [_]) = [e]
getExprs (Op _ exprs) = concatMap getExprs exprs 
getExprs (Val v) = [Val v]
getExprs (Var _) = error "Question is poorly phrased, a set is not a valid variable" -- If we have a set on its own in the expression, we throw an error
-- getExprs e = error ("invalid expression: " ++ exprToLatex e)