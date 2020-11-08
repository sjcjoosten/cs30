module CS30.Exercises.ProbExProof where
import CS30.Data
import CS30.Exercises.Data
import Control.Monad.Combinators.Expr
import Data.Functor.Identity
import Data.List
import Data.Ratio
import Data.Void
import Data.List.Extra
import Text.Megaparsec
import Text.Megaparsec.Char

data ExExpr =
            Econst Integer
            | Eranvar String
            | EbinOp Eop ExExpr ExExpr 
            | Ecov ExExpr ExExpr
            | E ExExpr 
            deriving (Eq,Ord,Show)

data Eop = Plus | Minus | Times
            deriving (Eq,Ord,Show)

type Parser = ParsecT Void String Identity

type Equation = (ExExpr, ExExpr)

data Law = Law String Equation 

data Proof = Proof ExExpr [Step]
type Step = (String, ExExpr)

-- | TODO: put all laws here!
laws :: [Law]
laws = [] -- map parse [bunch-of-strings]

-- | Rachael's code: generate a random expression
-- followed lecture
genRanEx :: Int -> ChoiceTree ExExpr
genRanEx i | i < 1 
   = Branch [ Branch [Node (Eranvar varName) | varName <- ["X","Y","Z"]]
            , Branch [Node (Econst val) | val <- [2..10]]
            ]
genRanEx i 
   = Branch [do e <- genRanEx (i-1)
                return (E e)
            , Branch [ do e1 <- genRanEx i'
                          e2 <- genRanEx (i - i' - 1)
                          op <- nodes [Plus, Minus, Times] 
                          return (EbinOp op e1 e2)
                     | i' <- [0..i-1] -- ensures e1 and e2 will sum to i
                     ]
            ]

-- | Rachael's code: parse an expression
-- the expression parser does law parser in reverse?
-- takes something like this:
-- [law1] (EbinOp Plus (EbinOp Plus (EbinOp Plus (Econst 0) (Econst 1)) (Econst 2)) (Econst 3))
-- and turns it into something like that:
-- ((0 + 1) + 2) + 3
parseExExpr :: Parser ExExpr
parseExExpr = undefined

-- | Rachael's code: parse a law
-- we want to do something like this:
-- genProof [law1] (EbinOp Plus (EbinOp Plus (EbinOp Plus (Econst 0) (Econst 1)) (Econst 2)) (Econst 3))
-- write this as ((0 + 1) + 2) + 3
-- so parser needs to turn this ^^ into that up there ^^
parseLawExExpr :: Parser Law
parseLawExExpr = undefined

-- | Joint code: combining all details
--   TODO: generate feedback (printing a proof if the answer is wrong)
probExProof :: ExerciseType
probExProof = exerciseType "ProbExProof" "L?.?" "Probability : Expected Value"
            probExercises --choiceTreeList
            genQuestion
            undefined -- genFeedback
-- | Joint code: three levels of exercises
--   TODO: after getting the completed proof,
--         randomly move terms from the result to the lhs and ask the user to compute the lhs.
probExercises :: [ChoiceTree ([Field], Integer)] -- first thing in field will be expression, rest its equivalencies
probExercises = [fullExercise 3, fullExercise 5, fullExercise 7]
  where fullExercise i = do ex <- genRanEx i
                            let Proof lhs steps = genProof laws ex
                                (_,rhs) = last steps
                            asgn <- assignRanVal rhs 
                            let answer = head (evaluate asgn rhs)
                            let exprs = nubSort (getExprs rhs)
                            if denominator answer == 1 then return () else error "Resulting answer in the generated puzzle is a fraction, that shouldn't have happened! (Error stems from Sebastiaan's code)" 
                            return (genFields lhs (evaluate asgn) exprs, numerator answer)
        showFrac ans | denominator ans == 1 = show (numerator ans)
        showFrac _ = error "Got a fraction in the generated puzzle, that shouldn't have happened! (Error stems from Sebastiaan's code)"
        genFields lhs getVal exprs
          = [FText "Given that "] 
            ++ combine [FMath (asLaTeX e ++ "=" ++ showFrac ans) | e <- exprs, ans <- getVal e]
            ++ [FText ". Compute ", FMath (asLaTeX lhs)] 

combine :: [Field] -> [Field]
combine [] = []
combine [x] = [x] 
combine [x,y] = [x, FText " and ", y]
combine [x,y,z] = [x, FText ", ", y, FText ", and ", z]
combine (x:xs) = [x, FText ", "] ++ combine xs

-- | take an expression, generate latex string
asLaTeX :: ExExpr -> String 
asLaTeX = undefined

-- randomSelect (Branch choiceTreeList)
genQuestion :: ([Field], a) -> Exercise -> Exercise
genQuestion (question, _) ex = ex{eQuestion = question ++ [FFieldMath "answer"]}

-- | Sebastiaan's code: generate proofs deterministically
genProof :: [Law] -> ExExpr -> Proof
genProof laws' e
 = Proof e (multiSteps e)
 where multiSteps e'
         = case [ (nm, res)
                | Law nm eq <- laws'
                , res <- getStep eq e'] of
             [] -> []
             ((nm,e''):_) -> (nm,e'') : multiSteps e''
-- | All ways to do a single proof step
getStep :: (ExExpr, ExExpr) -> ExExpr -> [ExExpr]
getStep (lhs, rhs) expr
 = [apply subst rhs | subst <- matchE lhs expr] ++
   case expr of
     (EbinOp o e1 e2) ->
        [EbinOp o e1  e2' | e2' <- getStep (lhs,rhs) e2]++
        [EbinOp o e1' e2  | e1' <- getStep (lhs,rhs) e1]
     (Ecov e1 e2) ->
        [Ecov e1  e2' | e2' <- getStep (lhs,rhs) e2]++
        [Ecov e1' e2  | e1' <- getStep (lhs,rhs) e1]
     (E e1) -> 
        [E e1' | e1' <- getStep (lhs,rhs) e1]
     (Econst _) -> []
     (Eranvar _) -> []

matchE :: ExExpr -> ExExpr -> [Subst]
matchE (Eranvar nm) expr = [[(nm,expr)]]
matchE (Econst i) (Econst j) | i == j = [[]]
matchE (Econst _i) _ = []
matchE (EbinOp o e1 e2) (EbinOp o' e1' e2') | o == o' 
 = [r | s1 <- matchE e1 e1'
      , s2 <- matchE e2 e2'
      , r <- combineSubsts s1 s2]
matchE (EbinOp _o _e1 _e2) _ = []
matchE (Ecov e1 e2) (Ecov e1' e2')
 = [r | s1 <- matchE e1 e1'
      , s2 <- matchE e2 e2'
      , r <- combineSubsts s1 s2]
matchE (Ecov _e1 _e2) _ = []
matchE (E e) (E e') = matchE e e'
matchE (E _e) _ = []

combineSubsts :: Subst -> Subst -> [Subst]
combineSubsts s1 s2
 = [substUnion s1 s2 | combinable s1 s2]
 where
   substUnion l1 l2 = l1 ++ l2
   combinable l1 l2 = and [v1==v2 | (k1,v1) <- l1, (k2,v2) <- l2, k1==k2]

apply :: Subst -> ExExpr -> ExExpr
apply lst = go
  where go (Eranvar nm)
          = case lookup nm lst of
              Just v -> v
              _ -> error "Couldn't find key in lookup"
        go (Econst i) = Econst i
        go (EbinOp o e1 e2)
         = EbinOp o (go e1) (go e2)
        go (Ecov e1 e2) = Ecov (go e1) (go e2)
        go (E e) = E (go e)

type Subst = [(String,ExExpr)]

-- takes inputs to evaluate expected value
type RanVals = [(String,[Rational])] -- For each variable, mention the value it takes for each event from the event space. Lists of integers should be equal in size.

-- | Sebastiaan's code: Evaluate expressions
-- Given the RanVals, this function returns a list of possible values (one for each event).
-- If the function is deterministic, this will return the sum value as a singleton list.
evaluate :: RanVals -> ExExpr -> [Rational]
evaluate lkp (Eranvar nm)
  = case lookup nm lkp of
        Nothing -> error "Evaluate was called with its first argument incomplete.."
        Just v -> v
evaluate lkp (E e1) = [avg $ evaluate lkp e1]
evaluate lkp (Ecov e1 e2) = [avgProd - avg1 * avg2]
  where avg1 = avg $ evaluate lkp e1
        avg2 = avg $ evaluate lkp e2
        avgProd = avg $ evaluate lkp (EbinOp Times e1 e2)
evaluate _ (Econst i) = [i%1]
evaluate lkp (EbinOp op e1 e2)
  = [f v1 v2 | (v1,v2) <- zipRotate (evaluate lkp e1) (evaluate lkp e2)]
  where f = case op of
              Plus -> (+)
              Minus -> (-)
              Times -> (*)
        zipRotate [l1] l2 = zip (repeat l1) l2
        zipRotate l1 l2 = zip l1 (cycle l2)

avg :: [Rational] -> Rational
avg lst = s / l
  where (s,l) = foldl' (\(t,n) v -> seq t (seq n (t+v,n+1))) (0,0) lst

-- | Generate a list of things of which we need to tell the user what the value is, based on what the expression was
getExprs :: ExExpr -> [ExExpr]
getExprs (Eranvar _) = error "Question is ill-phrased: a random variable is not a value"
getExprs (Econst _) = []
getExprs (EbinOp _ e1 e2) = getExprs e1 ++ getExprs e2
getExprs e@(E _) = [e]
getExprs e@(Ecov _ _) = [e]

-- | Sebastiaan's code: randomly obtain a valuation
assignRanVal :: ExExpr -> ChoiceTree RanVals
assignRanVal expr
  = go (nubSort $ variables expr)
  where variables (Eranvar nm) = [nm]
        variables (Econst _) = []
        variables (EbinOp _ e1 e2) = variables e1 ++ variables e2
        variables (Ecov e1 e2) = variables e1 ++ variables e2
        variables (E e) = variables e
        numvals = case getNumvals expr of
                    n | n <  7    -> n
                      | otherwise -> 10
        getNumvals (E e) = getNumvals e
        getNumvals (Eranvar _) = 1
        getNumvals (Econst _) = 0
        getNumvals (EbinOp op e1 e2)
           = (case op of
               Minus -> max
               Plus -> max
               Times -> (+)) (getNumvals e1) (getNumvals e2)
        getNumvals (Ecov e1 e2) = getNumvals e1 + getNumvals e2
        go [] = return []
        go (x:xs) = do vals <- getVals numvals
                       rm <- go xs
                       return ((x,vals):rm)
        getVals 0 = return []
        getVals n = do val <- nodes [2,3,4,5,6,9,10,12,15,20]
                       rm <- getVals (n-1)
                       return (val * (numvals % 1):rm)
