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
            deriving (Eq,Show)

data Eop = Plus | Minus | Times
            deriving (Eq,Show)

type Parser = ParsecT Void String Identity

type Equation = (ExExpr, ExExpr)

data Law = Law String Equation 

data Proof = Proof ExExpr [Step]
type Step = (String, ExExpr)

-- | TODO: put all laws here!
laws :: [Law]
laws = []

-- | Rachael's code: generate a random expression
genRanEx :: Int -> ChoiceTree ExExpr
genRanEx = undefined
-- | Rachael's code: parse an expression
parseExExpr :: Parser ExExpr
parseExExpr = undefined
-- | Rachael's code: parse a law
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
type ExValEval = [(String, Integer)]
-- takes inputs to evaluate covariance
type CoVarEval = [(String, String, Integer)]

-- | Sebastiaan's code: evaluate expressions
evaluate :: ExValEval -> CoVarEval -> ExExpr -> Integer
evaluate = undefined

-- | Sebastiaan's code: randomly obtain a valuation
assignRanVal :: ExExpr -> ChoiceTree (ExValEval, CoVarEval)
assignRanVal = undefined
