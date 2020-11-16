module CS30.Exercises.LogicRewriting.ProofGeneration where

import CS30.Exercises.LogicRewriting.Parsing (Law (..), Expr (..), Equation)

data Proof = Proof Expr [Step] deriving Show
type Step = (String, Expr) -- law name and resulting expression 

-- generate a proof based on provided laws and initial expression
getDerivation :: [Law] -> Expr -> Proof
getDerivation laws e = Proof e (manyStep laws e)

-- generates all the steps in the proof
-- by repeatedly using the laws to rewrite expressions
manyStep :: [Law] -> Expr -> [Step]
manyStep laws e = if null steps then []
                  else step : manyStep laws (snd step)
                  where steps = [(name,e') | Law name eqn <- laws,
                                             e' <- rewrites eqn e,
                                             e' /= e]
                        step = head steps

-- rewrites gives all the ways of rewriting given expression
-- using only the provided equation (law)
rewrites :: Equation -> Expr -> [Expr]
rewrites (lhs, rhs) expr = 
    case matchExpr lhs expr of
        []         -> recurse expr
        (sub:subs) -> [apply s rhs | s <- (sub:subs)]
    where recurse (Var _)         = []
          recurse (Const _)       = []
          recurse (Neg e)         = [Neg e' | e' <- rewrites (lhs,rhs) e]
          recurse (Bin op e1 e2)  = [Bin op e1' e2 | e1' <- rewrites (lhs, rhs) e1] ++
                                    [Bin op e1 e2' | e2' <- rewrites (lhs, rhs) e2]

type Subst = [(Char, Expr)]

-- returns the substitutions necessary to transform the first expression
-- into the second expression
-- returns [] if the expressions don't match
-- and [[]] if the expressions match, but don't require any substitutions 
matchExpr :: Expr -> Expr -> [Subst]
matchExpr (Var v) e = [ [(v,e)] ]
matchExpr (Const i) (Const j) | i == j = [[]]
matchExpr (Const _) _ = []
matchExpr (Neg e1) (Neg e2) = matchExpr e1 e2
matchExpr (Neg _) _  = []
matchExpr (Bin op1 e1 e2) (Bin op2 e3 e4)
    | op1 == op2 = combine (matchExpr e1 e3) (matchExpr e2 e4)
    | otherwise  = []
matchExpr (Bin _ _ _) _ = []

-- combines two lists of substitutions, checking whether
-- substitutions are compatible (i.e. each variable is mapped to
-- exactly one expression) 
combine :: [Subst] -> [Subst] -> [Subst]
combine xs ys = [x ++ y | x <- xs, y <- ys, compatible x y] 
                where compatible x y = and [e1 == e2 
                                           | (v1,e1) <- x, 
                                             (v2,e2) <- y,
                                             v1 == v2]

-- apply a substitution to transform one expression to another
apply :: Subst -> Expr -> Expr
apply subst  (Var v)         = lookupInSubst v subst
apply _subst (Const b)       = Const b
apply subst  (Neg e)         = Neg (apply subst e)
apply subst  (Bin op e1 e2)  = Bin op (apply subst e1) (apply subst e2)

-- return the expression to transform variable "v" into
lookupInSubst :: Char -> Subst -> Expr
lookupInSubst v ((v', expr):rm)
    | v == v'   = expr
    | otherwise = lookupInSubst v rm
lookupInSubst _ [] = error "Substitution was not complete"
