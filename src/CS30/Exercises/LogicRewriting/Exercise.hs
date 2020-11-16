module CS30.Exercises.LogicRewriting.Exercise where

import           CS30.Data
import           CS30.Exercises.Data
import           CS30.Exercises.Util
import qualified Data.Map as Map
import           Data.List
import           CS30.Exercises.LogicRewriting.Parsing (laws, Expr (..), Op (..), Law (..))
import           CS30.Exercises.LogicRewriting.ProofGeneration (getDerivation, Proof (..), apply, Step)

-- final exercise type
logicRewritingEx :: ExerciseType
logicRewritingEx = exerciseType "Logic Rewriting" "L?.?" "Logic Rewriting"
                       logicExercises
                       logicQuer
                       logicFeedback

-- generate initial expressions to put through the prover
-- using the provided function for generating expressions
initialExpr :: (Int -> ChoiceTree Expr) -> ChoiceTree Expr
initialExpr expr = neg <$> expr 7
                   where neg (Neg e)   = Neg e
                         neg otherExpr = Neg otherExpr
                         -- ^ we don't want to negate an expression that 
                         -- already begins with a negation

-- random expressions of given size, using variables 'p','q', and 'r'
exprOfSize :: Int -> ChoiceTree Expr
exprOfSize 1 = Branch [nodes (map Var ['p','q','r'])]
exprOfSize 2 = Branch [Neg <$> exprOfSize 1]
exprOfSize n = Branch ([Bin op <$> exprOfSize i <*> exprOfSize (n-i-1)
                       | i <- [1..(n-2)]
                       , op <- [And, Or, Implies]])

-- extracts all the variables from an expression
getVars :: Expr -> [Char]
getVars (Var v) = [v]
getVars (Const _) = []
getVars (Neg e) = getVars e
getVars (Bin _ e1 e2) = getVars e1 ++ getVars e2

-- gets the size of an expression
-- (variables have size 0, because in the case of matching laws, 
-- they will be replaced by other expressions)
getSize :: Expr -> Int
getSize (Var _)         = 0
getSize (Const _)       = 1
getSize (Neg e)         = 1 + getSize e
getSize (Bin _ e1 e2)   = 1 + getSize e1 + getSize e2

-- assigns random expressions of random sizes to the variables
-- the expressions assigned should have sizes that sum to the given "size",
--  thereby constraining the size of the expression overall
assignRandExprs :: Int -> [Char] -> ChoiceTree [(Char, Expr)]
assignRandExprs size vars | length vars == 0 = Node []
                          | otherwise = Branch $ map (mapM genPair) (map (zip vars) exprSizes)
                          where -- exprSizes gives the possible sizes for substitutions,
                              -- in order to sum to the desired size
                              exprSizes = sumLen size (length vars)
                              -- sumLen returns all lists of positive integers of length l
                              -- that sum to n
                              sumLen :: Int -> Int -> [[Int]]
                              sumLen n 1 = [[n]]
                              sumLen n l = [num:r | num <- [1..(n-1)], r <- sumLen (n-num) (l-1)]
                              genPair :: (Char, Int) -> ChoiceTree (Char, Expr)
                              genPair (c, n) = (exprOfSize n) >>= (\expr -> return (c, expr))

-- generate an expression to fit the law given by the expression
--  using s as a constraint on the size
-- 
-- note: if there are duplicate variables in the expression, we may generate 
--  an expression larger than s, but these should simplify quickly
forceLaw :: Expr -> Int -> ChoiceTree Expr
forceLaw e s = do let vars = nub $ getVars e
                  let size = s - (getSize e)
                  assignments <- assignRandExprs size vars
                  return (apply assignments e)

-- get all the laws which have given name
getLawsByName :: String -> ChoiceTree Law
getLawsByName name = nodes [law | law <- laws, lawName law == name]

-- utility function, places an element into the list 
-- at given index (assumes index is valid for the list) 
putElemIn :: a -> Int -> [a] -> [a]
putElemIn y 0 xs = y:xs
putElemIn y _ [] = y:[]
putElemIn y n (x:xs) = x:(putElemIn y (n-1) xs)

-- the laws we want to test, in order
-- (note: we don't test Definition of False/True at all, 
--  although we do use it in proofs)
testLaws :: [String]
testLaws = ["Double Negation Law",
            "De Morgan's Law",
            "Identity Law",
            "Domination Law",
            "Idempotent Law",
            "Negation Law",
            "Implication Law"
            ]

-- contains all the exercises: 
--  the first list of Fields is what we display as a question
--  the Int is the index of the right choice
--  and the second list of Fields is what we display as feedback
logicExercises :: [ChoiceTree ([Field], Int, [Field])]
logicExercises = [do law <- getLawsByName name
                     let lhs = fst $ lawEqn law
                     e <- initialExpr (forceLaw lhs)
                     let (Proof e' steps) = getDerivation (laws' law) e
                     remStep <- nodes $ findIndices ((== name) . fst) steps
                     let stepName = fst $ steps!!remStep
                     choices <- (getOrderedSubset (delete stepName testLaws) 2)
                     correctN <- nodes [0..2]
                     let shuffChoices = putElemIn stepName correctN choices 
                     return (showExer (Proof e' steps) remStep shuffChoices, 
                             correctN,
                             showSol law)
                  | name <- testLaws]
                 where laws' law = if lawName law == "De Morgan's Law" then law:laws
                                   else laws
                       -- ^ De Morgan's Laws need to be put before other laws when they are being tested, 
                       -- because otherwise we will occasionally generate a proof that bypasses them
                       displayStepsExcept :: Int -> [Step] -> [String] -> [Field]
                       displayStepsExcept _ [] _  = []
                       displayStepsExcept n (s:rems) choices = [FMath "\\equiv", name, 
                                                                FIndented 1 [FMath $ show (snd s)]]
                                                               ++ displayStepsExcept (n-1) rems choices
                                                               where correct = FText ("{ "++(fst s)++" }")
                                                                     name = if n/=0 then correct
                                                                            else FChoice "choice" (map (\x -> [FText $ "{ "++x++" }"]) choices)
                       showExer :: Proof -> Int -> [String] -> [Field] 
                       showExer (Proof e steps) remStep choices = [FIndented 1 [FMath $ show e]] 
                                                                  ++ (displayStepsExcept remStep steps choices)
                       showSol :: Law -> [Field]
                       showSol (Law name (e1,e2)) = [FText $ "\""++name++": ", 
                                                     FMath $ show e1, FMath "\\equiv", FMath $ show e2, 
                                                     FText "\""]

-- generate the question displayed to the user
logicQuer :: ([Field], Int, [Field]) -> Exercise -> Exercise
logicQuer (quer, _, _) defExer = defExer {eQuestion = [FText "Can you name the missing law applied in this proof?"]
                                                      ++quer, eBroughtBy = ["Chibuzo", "Bennett"]}

-- generate feedback
logicFeedback :: ([Field], Int, [Field]) -> Map.Map String String -> ProblemResponse -> ProblemResponse
logicFeedback (_, sol, solFeedback) mStrs defaultRsp 
    = case rsp of 
        Just v -> if v == show (sol) then markCorrect $ defaultRsp{prFeedback = [FText "The correct answer was: "]++solFeedback}
                  else if v == "" then tryAgain $ defaultRsp{prFeedback = [FText "Please select which law is applied"]}
                       else markWrong $ defaultRsp{prFeedback = [FText "The correct answer was: "]++solFeedback} 
        Nothing -> tryAgain $ defaultRsp{prFeedback = [FText "We could not understand your answer"]}
      where rsp = Map.lookup "choice" mStrs
