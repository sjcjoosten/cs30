module CS30.Exercises.SetCardinalitiesProofs.CardinalityProof where
import CS30.Exercises.SetCardinalitiesProofs.RuleParser

type Step = (String, Expr)
type Steps = [Step]

type Laws = [Law]
type Expressions = [Expr]
type Substitution = [(Char, Expr)]

data Proof = Proof Expr Steps deriving (Show, Eq)

law1 = Law {lawName = "TestLaw", lawEquation = (Op Cardinality [Op Union [Var 'A',Var 'B']],Op Sub [Op Add [Op Cardinality [Var 'A'],Op Cardinality [Var 'B']],Op Cardinality [Op Intersection [Var 'A',Var 'B']]])}

lookupInSubstitution :: Char -> Substitution -> Expr
lookupInSubstitution name ((nm, v):rm)
    | name == nm = v
    | otherwise = lookupInSubstitution name rm
lookupInSubstitution _ [] = error "Substitution was not complete"

combineTwoSubs :: Substitution -> Substitution -> Maybe Substitution
combineTwoSubs sub1 sub2
    = case and [v1 == v2 | (name1, v1) <- sub1, (name2, v2) <- sub2, name1 == name2] of
          True -> Just (sub1 ++ sub2)
          False -> Nothing

match :: Expr -> Expr -> Maybe Substitution
match (Var name) expr = Just [(name, expr)]
match (Op op1 exprs1) (Op op2 exprs2)
    | op1 == op2 = 
        combineAll (zipWith match exprs1 exprs2)
    | otherwise = Nothing
match (Op _ _) (Var _) = Nothing


combineAll :: [Maybe Substitution] -> Maybe Substitution
combineAll [] = Just []
combineAll (Nothing:_) = Nothing
combineAll (Just x:xs) = case combineAll xs of
                            Nothing -> Nothing
                            Just s  -> combineTwoSubs x s
                

apply :: Substitution -> Expr -> Expr
apply sub (Var name) = lookupInSubstitution name sub
apply sub (Op symb exprs) = Op symb (map (apply sub) exprs) -- Ask how to handle applying for Op Expr

getStep :: Equation -> Expr -> Expressions
getStep (lhs, rhs) expr
    = case match lhs expr of
        Nothing -> recurse expr
        Just sub -> [apply sub rhs]
    where recurse (Var _) = []
          recurse (Op symb exprs) = [Op symb (context e')| (e, context) <- takeOneOf exprs, e' <- getStep (lhs,rhs) e ] -- Ask how to modify getStep for handling Op Expr

takeOneOf :: [a] -> [(a, a -> [a])]
takeOneOf [] = []
takeOneOf (a:as) = (a,(:as)): map f (takeOneOf as)
    where
        f (a', fn) = (a',(a:) . fn) -- a is put in front of the result of fn



getDerivation :: Laws -> Expr -> Proof
getDerivation laws expr = Proof expr (multiSteps expr)
    where multiSteps expr' 
            = case ( [(lawName law, resultingStep) 
                   | law <- laws
                   , resultingStep <- getStep (lawEquation law) expr'
                   ]) of
                [] -> []
                ((name, expr'') : _) -> (name, expr'') : multiSteps expr''