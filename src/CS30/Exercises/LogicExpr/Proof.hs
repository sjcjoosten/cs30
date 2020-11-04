{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.LogicExpr.Proof where
import CS30.Exercises.LogicExpr.Parser hiding (law)
import Data.Maybe ( fromJust )

type Substitution = [(VarName,LogicExpr)]
type VarName = Char

data Proof = Proof LogicExpr [Step] deriving (Show)

type Step = (LawName, LogicExpr)

input_laws :: [String]
input_laws = [
    "Law1:¬(¬p)≡p"
    , "Law2:p∧p≡p"
    , "Law3:p∧true≡p"
    , "Law4:p∨true≡true"
    , "Law5:p∨p≡p"
    , "Law6:(p ⇒ q) ≡ ¬p ∨ q"
    , "Law7:p∨false≡p"
    , "Law8:p∧false≡false"
    , "Law9:¬(p∨q)≡¬p∧¬q"
    , "Law10:¬(p∧q)≡¬p∨¬q"
    , "Law11:p∧¬p≡false"
    , "Law12:p∨¬p≡true"
    , "Redundancy law: (p∨q)∧(p∨¬q)≡p"
    , "Redundancy law: p∨(¬p∧q)≡p∨q"
    , "true/false: ¬true≡false"
    , "true/false: ¬false≡true"
    , "Assoc: (p∨q)∨r≡p∨q∨r"
    , "Assoc: p∨(q∨r)≡p∨q∨r"    
    , "Assoc: (p∧q)∧r≡p∧q∧r"
    , "Assoc: p∧(q∧r)≡p∧q∧r"
    -- Commutative laws give potentially verbose proof
    -- , "Commute: p∧q≡q∧p"
    -- , "Commute: p∨q≡q∨p"
    ]


simplify :: [String] -> String -> Proof
simplify strings str = getDerivation laws e
  where
    laws = map parseLaw strings
    e = parseExpr str

getDerivation :: [Law] -> LogicExpr -> Proof
getDerivation laws e
 = Proof e (multiSteps e []) -- Give multiSteps an accumulater param, for the use of commutative law;
 where multiSteps e' acc
        = case [ (lawName law, res)
               | law <- laws
               , res <- getStep (lawEq law) e'               
               ] of
            [] -> []
            xs ->  case chooseNew xs acc of
              Nothing -> []
              Just (nm, e'') -> (nm, e''): multiSteps e'' (e'':acc)
              where                 
                chooseNew [] _ = Nothing
                chooseNew ((name, x):xs') acc' = if x `elem` acc then chooseNew xs' acc' else Just (name, x)        


getStep :: Equation -> LogicExpr -> [LogicExpr]
getStep (lhs, rhs) expr
  = case matchE lhs expr of
      Nothing -> recurse expr
      Just subst -> [apply subst rhs]
  where recurse (Var _) = []
        recurse (Con _) = []
        recurse (Bin op e1 e2)
          = [Bin op e1' e2 | e1' <- getStep (lhs,rhs) e1] ++
            [Bin op e1 e2' | e2' <- getStep (lhs,rhs) e2]
        recurse (Neg e1)
          = [Neg e1' | e1' <- getStep (lhs,rhs) e1]


matchE :: LogicExpr -> LogicExpr -> Maybe Substitution
matchE (Var nm) expr = Just [(nm,expr)]
matchE (Con i) (Con j) | i == j = Just []
matchE (Con _) _ = Nothing

-- matchE (Bin op1 e1 e2) (Bin op2 e3 e4) | op1 == op2 
--  = case (matchE e1 e3, matchE e2 e4) of
--     (Just s1, Just s2) -> combineTwoSubsts s1 s2
--     _ -> Nothing

matchE (Bin op1 e1 e2) (Bin op2 e3 e4) | op1 == op2 
 = case tryMatch e1 e2 e3 e4 of -- And and Or operators are commutative, so check for reversed match
          Nothing -> if op1 == And || op1 == Or then tryMatch e1 e2 e4 e3 else Nothing
          res -> res
   where 
     tryMatch e1' e2' e3' e4'= do
          s1 <- matchE e1' e3'
          s2 <- matchE e2' e4'
          combineTwoSubsts s1 s2 

matchE (Bin _ _ _) _ = Nothing
matchE (Neg e1) (Neg e2) = matchE e1 e2
matchE (Neg _) _ = Nothing

combineTwoSubsts :: Substitution -> Substitution -> Maybe Substitution
combineTwoSubsts s1 s2
  = case and [v1 == v2 | (nm1,v1) <- s1, (nm2,v2) <- s2, nm1 == nm2] of
     True -> Just (s1 ++ s2)
     False -> Nothing

apply :: Substitution -> LogicExpr -> LogicExpr
apply subst (Var nm) = fromJust $ lookup nm subst
apply _ (Con i) = Con i
apply subst (Neg e) = Neg (apply subst e)
apply subst (Bin op e1 e2) = Bin op (apply subst e1) (apply subst e2)