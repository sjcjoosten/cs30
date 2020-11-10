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
                  -- ^ TODO: should we change the condition here?
                  --   since our ultimate goal is to propagate negation inwards?
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
          -- | TODO: refactor data structure with some sort of BinOp to clean this up
          recurse (And e1 e2)     = [And e1' e2 | e1' <- rewrites (lhs, rhs) e1] ++
                                    [And e1 e2' | e2' <- rewrites (lhs, rhs) e2]
          recurse (Or e1 e2)      = [Or e1' e2 | e1' <- rewrites (lhs, rhs) e1] ++
                                    [Or e1 e2' | e2' <- rewrites (lhs, rhs) e2]
          recurse (Implies e1 e2) = [Implies e1' e2 | e1' <- rewrites (lhs, rhs) e1] ++
                                    [Implies e1 e2' | e2' <- rewrites (lhs, rhs) e2]

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
matchExpr (And e1 e2) (And e3 e4)
    = combine (matchExpr e1 e3) (matchExpr e2 e4)
matchExpr (And _ _) _  = []
matchExpr (Or e1 e2) (Or e3 e4) 
    = combine (matchExpr e1 e3) (matchExpr e2 e4)
matchExpr (Or _ _) _  = []
matchExpr (Implies e1 e2) (Implies e3 e4) 
    = combine (matchExpr e1 e3) (matchExpr e2 e4)
matchExpr (Implies _ _) _  = []

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
apply subst  (And e1 e2)     = And (apply subst e1) (apply subst e2)
apply subst  (Or e1 e2)      = Or (apply subst e1) (apply subst e2)
apply subst  (Implies e1 e2) = Implies (apply subst e1) (apply subst e2)

-- return the expression to transform variable "v" into
lookupInSubst :: Char -> Subst -> Expr
lookupInSubst v ((v', expr):rm)
    | v == v'   = expr
    | otherwise = lookupInSubst v rm
lookupInSubst _ [] = error "Substitution was not complete"
