module CS30.Exercises.SetCardinalitiesProofs.ExerciseGeneration where
import CS30.Data
import CS30.Exercises.Data
import Data.Char
import CS30.Exercises.Util
import CS30.Exercises.SetCardinalitiesProofs.Proof
import CS30.Exercises.SetCardinalitiesProofs.RuleParser
import qualified Data.Map as Map
import Data.List.Extra
import Text.Megaparsec

type SetCardinalityProblem = ([Field], Proof, Integer)

 -- Stores (expression, possible values) for each expression found in the equation
type PossibleVals = [(Expr, Integer)]

cardinalityProofExer :: ExerciseType
cardinalityProofExer 
    = exerciseType  "Set Cardinality" "L?.?" 
                    "Sets: Cardinalities"
                    setExercises
                    generateQuestion
                    generateFeedback


setExercises :: [ChoiceTree SetCardinalityProblem] -- first thing in field will be expression, rest its equivalencies
setExercises = [fullExercise 3] -- removed larger exercises as they cause negative exponent errors
  where fullExercise i = do ex <- generateRandSetExpr i
                            let proof@(Proof lhs steps) = genProof laws (Op Cardinality [ex])
                                rhs = last (lhs:map snd steps)
                            asgn <- genAllPossibleValues rhs 
                            let answer = evaluate asgn rhs -- negative exponents happen here (but only sometimes!)
                            let exprs = (getExprs rhs)
                            return (genFields lhs (evaluate asgn) exprs, proof, answer)

        genFields lhs getVal exprs
          = [FText "Given that "] 
            ++ combine [FMath (exprToLatex e ++ "=" ++ show (getVal e)) | e <- exprs, notVal e]
            ++ [FText ". Compute ", FMath (exprToLatex lhs)]
            ++ [FFieldMath "Answer"]

        notVal (Val _) = False
        notVal _ = True


-- Shows the question
generateQuestion :: SetCardinalityProblem -> Exercise -> Exercise
generateQuestion (myProblem, _, _) def 
    = def { eQuestion =  myProblem 
          , eBroughtBy = ["Paul Gralla","Joseph Hajjar","Roberto Brito"] }


-- function which shows the feedback depending on incorrect/correct answer
generateFeedback :: SetCardinalityProblem -> Map.Map String String -> ProblemResponse -> ProblemResponse
generateFeedback (_, (Proof proofExpr proofSteps), answer) usrRsp pr 
      = reTime$ case answer' of
                  Nothing -> wrong{prFeedback=(FText " Ill formatted reponse, please only input integers, answer was "):rspwa} -- Error when parsing. 
                  Just v -> if (v == fromIntegral answer) 
                              then correct{prFeedback=correctRsp}
                              else wrong{prFeedback=wrongRsp} 
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
                Nothing -> [FText "Could not read input."]
                Just v -> [FMath v, FText "."]

            wrongRsp = [FText ("incorrect response, these are the steps to calculate"), FMath (exprToLatex proofExpr ++ "=" ++ show answer), FText "with the given values",
                        FTable (
                            [headerRow] ++           
                            [[Cell (FText "Expression") , Cell (FMath ("\\quad" ++ exprToLatex proofExpr))]] ++
                            [[Cell (FText rule) , Cell (FMath ("= " ++ exprToLatex step))] | (rule, step) <- proofSteps]                                                                          -- add header row to the table                        
                        )
                    ]
            headerRow = [Header (FText "Rule"), Header (FText "Deduction Step")]

-- joins together sequential fields
combine :: [Field] -> [Field]
combine [] = []
combine [x] = [x] 
combine [x,y] = [x, FText " and ", y]
combine [x,y,z] = [x, FText ", ", y, FText ", and ", z]
combine (x:xs) = [x, FText ", "] ++ combine xs

-- Function to generate a random set expression
generateRandSetExpr :: Int -> ChoiceTree Expr
generateRandSetExpr i = fst . flip setVars ['A' ..] <$> (generateRandSetExpr_helper [Union, Powerset, Cartesian, Setminus] i)
    
-- Loops through a random set expression and names variables ordinally in order to ensure no duplicates
setVars :: Expr -> [Char] -> (Expr, [Char])
setVars e@(Val _) varNames = (e, varNames)
setVars (Var _) (possible:possibles) = (Var possible, possibles)
setVars (Op o [expr]) possibles = 
        let (expr', newPossible) = setVars expr possibles in (Op o [expr'], newPossible)
setVars (Op o [expr1,expr2]) possibles = 
        let (expr1', newPossible1) = setVars expr1 possibles
            (expr2', newPossible2) = setVars expr2 newPossible1
        in
            (Op o [expr1',expr2'], newPossible2)
setVars _ _ = error "Invalid input given to setVars"

-- Ensures that only the correct operations are considered with respect to depth
generateRandSetExpr_helper :: [Symb] -> Int -> ChoiceTree Expr
generateRandSetExpr_helper _ n 
    | n < 2 = Branch [ Node (Var varName) | varName <- ['A']]
generateRandSetExpr_helper lstOfOps n = do {
                            symb <- nodes lstOfOps;
                            -- If we have a Union, only consider Unions
                            if symb == Union then
                                do {
                                    n' <- nodes [1 .. n-1];
                                    expr1 <- generateRandSetExpr_helper [Union] n';
                                    expr2 <- generateRandSetExpr_helper [Union] (n - n' - 2);

                                    return (Op symb [expr1, expr2])
                                }
                            -- If we have a Powerset, only consider Union and Setminus
                            else if symb == Powerset then
                                do {
                                    expr <- generateRandSetExpr_helper [Union, Setminus] (n-1);
                                    return (Op symb [expr])
                                }
                            -- Union and setminus are the only expressions we allow after the top level operator
                            else 
                                do {
                                    n' <- nodes [1 .. n-1];
                                    expr1 <- generateRandSetExpr_helper [Union, Setminus] n';
                                    expr2 <- generateRandSetExpr_helper [Union, Setminus] (n - n' - 2);
                                    return (Op symb [expr1, expr2])                                
                                }
} 


-- From IncExcCardinalities.hs to get the user's answer, tried to import but seems like it was not exposed/exported by the module
parseAnswer :: Parser Int
parseAnswer = unspace digits
  where 
      digits = do ds <- some digitP
                  return (foldl1 shiftl ds)
      shiftl m n = 10*m+n
      digitP = cvt <$> satisfy isDigit
      cvt d = fromEnum d - fromEnum '0'


-- Generates all of the possible values for the cardinality of an expression
genAllPossibleValues :: Expr -> ChoiceTree PossibleVals
genAllPossibleValues expr = assignAll toAssign
    where
        toAssign = nubSort (getExprs expr)  -- List of all expression (e.g. Var A, Op Intersection [Var A, Var C], Op Intersection [Var B,Var C] ] )
        assignAll [] = Node []
        assignAll (x:xs) = do {
            -- Make sure that all intersection cardinalities are less than individual set cardinalities
            intersectionPossibleVal <- nodes [2 .. 10]; -- Intersection cardinality can only be in [2,10]
            varPossibleVal <- nodes [11 .. 20];         -- Individual set cardinality can only be in [11,20]
            genericPossibleVal <- nodes [2 .. 20];      -- Other cardinalities can be anywhere in [2,20]
            xs' <- assignAll xs;
            case (x) of
                Val (x')                            -> return ((x, x'):xs'); -- Only possible value for a constant is itself
                Op Cardinality [(Var _)]            -> return ((x, varPossibleVal):xs');
                Op Cardinality [(Op Intersection _)]-> return ((x, intersectionPossibleVal):xs');
                Op Cardinality [(Op Setminus _)]    -> return ((x, intersectionPossibleVal):xs');
                _                                   -> return ((x, genericPossibleVal):xs');
        }


-- Evaluates an expression with possible values to give an integer answer
evaluate :: PossibleVals -> Expr -> Integer
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
                            

-- Gets the expressions which will need values assigned to them/have values assigned to them
getExprs :: Expr -> [Expr]
getExprs e@(Op Cardinality [_]) = [e]
getExprs (Op _ exprs) = concatMap getExprs exprs 
getExprs (Val v) = [Val v]
getExprs (Var _) = error "Question is poorly phrased, a set is not a valid variable" -- If we have a set on its own in the expression, we throw an error