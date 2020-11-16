{-# LANGUAGE TemplateHaskell #-}
module CS30.Exercises.SetCardinalitiesProofs.Proof where
import CS30.Exercises.SetCardinalitiesProofs.RuleParser
import Data.Aeson as JSON
import Data.Aeson.TH

type Step = (String, Expr)
type Steps = [Step]

type Laws = [Law]
type Expressions = [Expr]
type Substitution = [(Char, Expr)]

data Proof = Proof Expr Steps deriving (Show,Eq)
$(deriveJSON defaultOptions ''Proof)

-- converts a proof to a string
proofToString :: Proof -> String
proofToString (Proof expr steps) = "Proof: " ++ exprToLatex expr ++ "\n" ++ helper steps
    where 
        helper ((s,expression):xs) = s ++ ":  " ++ exprToLatex expression ++ "\n" ++ helper xs
        helper [] = ""

-- Sebastians Code
genProof :: [Law] -> Expr -> Proof
genProof laws' e
 = Proof e (multiSteps e)
 where multiSteps e'
         = case [ (nm, res)
                | Law nm eq <- laws'
                , res <- getStep eq e'] of
             [] -> []
             ((nm,e''):_) -> (nm,e'') : multiSteps e''

-- function which returns the expression given the name in the (char,expr) tuple
lookupInSubstitution :: Char -> Substitution -> Expr
lookupInSubstitution name ((nm, v):rm)
    | name == nm = v
    | otherwise = lookupInSubstitution name rm
lookupInSubstitution _ [] = error "Substitution was not complete"

-- combines two substitutions if their names are the seame
combineTwoSubs :: Substitution -> Substitution -> Maybe Substitution
combineTwoSubs sub1 sub2
    = case and [v1 == v2 | (name1, v1) <- sub1, (name2, v2) <- sub2, name1 == name2] of
          True -> Just (sub1 ++ sub2)
          False -> Nothing

-- matches two expressions to return substitutions
match :: Expr -> Expr -> Maybe Substitution
match (Val _) _ = Nothing
match _ (Val _) = Nothing
match (Var name) expr = Just [(name, expr)]
match (Op op1 exprs1) (Op op2 exprs2)
    | op1 == op2 = 
        combineAll (zipWith match exprs1 exprs2)
    | otherwise = Nothing
match (Op _ _) (Var _) = Nothing

-- combines a list of substitutions
combineAll :: [Maybe Substitution] -> Maybe Substitution
combineAll [] = Just []
combineAll (Nothing:_) = Nothing
combineAll (Just x:xs) = case combineAll xs of
                            Nothing -> Nothing
                            Just s  -> combineTwoSubs x s

-- applies a substitution
apply :: Substitution -> Expr -> Expr
apply _ (Val v) = (Val v)
apply sub (Var name) = lookupInSubstitution name sub
apply sub (Op symb exprs) = Op symb (map (apply sub) exprs)

-- Rewrites an expression if it matches the form of a law
getStep :: Equation -> Expr -> Expressions
getStep (lhs, rhs) expr
    = case match lhs expr of
        Nothing -> recurse expr
        Just sub -> [apply sub rhs]
    where recurse (Var _) = []
          recurse (Val _) = []
          recurse (Op symb exprs) = [Op symb (context e')| (e, context) <- takeOneOf exprs, e' <- getStep (lhs,rhs) e ]

-- Used to pair an expression with potential rewritten forms
takeOneOf :: [a] -> [(a, a -> [a])]
takeOneOf [] = []
takeOneOf (a:as) = (a,(:as)): map f (takeOneOf as)
    where
        f (a', fn) = (a',(a:) . fn) -- a is put in front of the result of fn

-- Returns a proof given laws and expressions
getDerivation :: Laws -> Expr -> Proof
getDerivation lawSet expr = Proof expr (multiSteps expr)
    where multiSteps expr' 
            = case ( [(lawName law, resultingStep) 
                   | law <- lawSet
                   , resultingStep <- getStep (lawEquation law) expr'
                   ]) of
                [] -> []
                ((name, expr'') : _) -> (name, expr'') : multiSteps expr''
