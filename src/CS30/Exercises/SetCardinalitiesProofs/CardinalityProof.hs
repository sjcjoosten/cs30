module CS30.Exercises.SetCardinalitiesProofs.CardinalityProof where
import CS30.Exercises.SetCardinalitiesProofs.RuleParser

type Step = (String, Expr)
type Steps = [Step]

type Laws = [Law]
type Expressions = [Expr]
type Substitution = [(Char, Expr)]

data Proof = Proof Expr Steps

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
match (Op symb1 exprs1) (Op symb2 exprs2) = undefined -- Ask how to handle matching two Op Exprs
match (Op _ _) (Var _) = Nothing

apply :: Substitution -> Expr -> Expr
apply sub (Var name) = lookupInSubstitution name sub
apply sub (Op symb (expr : exprs)) = Op symb ((apply sub expr):exprs) -- Ask how to handle applying for Op Expr
apply _ (Op _ []) = undefined -- Ask how to handle empty set for Op Expr

getStep :: Equation -> Expr -> Expressions
getStep (lhs, rhs) expr
    = case match lhs expr of
        Nothing -> recurse expr
        Just sub -> [apply sub rhs]
    where recurse (Var _) = []
          recurse (Op symb (ex : exs)) = undefined -- Ask how to modify getStep for handling Op Expr
          recurse (Op _ []) = []

getDerivation :: Laws -> Expr -> Proof
getDerivation laws expr = Proof expr (multiSteps expr)
    where multiSteps expr' 
            = case ( [(lawName law, resultingStep) 
                   | law <- laws
                   , resultingStep <- getStep (lawEquation law) expr'
                   ]) of
                [] -> []
                ((name, expr'') : _) -> (name, expr'') : multiSteps expr''