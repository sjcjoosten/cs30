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
    "Negation of Negation:¬(¬p)≡p"
    , "Idempotence:p∧p≡p"
    , "Operation with true,false:p∧true≡p"
    , "Operation with true,false:p∨true≡true"
    , "Idempotence:p∨p≡p"
    , "Implications as an OR:(p ⇒ q) ≡ ¬p ∨ q"
    -- Easy challenge done right??
    -- , "Implications as an OR:¬(p⇒q)≡p∧q"
    , "Operation with true,false:p∨false≡p"
    , "Operation with true,false:p∧false≡false"
    , "DeMorgan's Law:¬(p∨q)≡¬p∧¬q"
    , "DeMorgan's Law:¬(p∧q)≡¬p∨¬q"
    , "Operation with Negation:p∧¬p≡false"
    , "Operation with Negation:p∨¬p≡true"
    -- Below are not in the given law list 
    , "Redundancy law: (p∨q)∧(p∨¬q)≡p"
    , "Redundancy law: p∨(¬p∧q)≡p∨q"
    , "____: ¬true≡false"
    , "____: ¬false≡true"
    , "Associativity1: (p∨q)∨r≡p∨(q∨r)"
    , "Associativity2: p∨(q∨r)≡(p∨q)∨r"    
    , "Associativity3: (p∧q)∧r≡p∧(q∧r)"
    , "Associativity4: p∧(q∧r)≡(p∧q)∧r"
    -- Commutative laws give potentially verbose proof
    , "Commutativity: p∧q≡q∧p"
    , "Commutativity: p∨q≡q∨p"
    ]

fake_laws :: [String]
fake_laws = [
    "FAKELAW: ¬(p ⇒ q) ≡ ¬p ⇒ ¬q"
    , "FAKELAW: p∨(q∧r)≡(p∨q)∧r"
  ]

data FaultyProof = FaultyProof LogicExpr [BoolStep] deriving (Show)
type BoolStep = (Bool, LogicExpr)

-- try to generate a faulty proof for an expr
-- return Nothing if a faulty proof not found (eg, fake laws not applied), otherwise return the FaultyProof
genFaultyProof :: LogicExpr -> Maybe FaultyProof
genFaultyProof e = if and $ map fst boolsteps then Nothing else Just (FaultyProof e boolsteps)
  where 
    laws = map parseLaw (fake_laws ++ input_laws )            
    (Proof _e steps) = getDerivation laws e
    boolsteps :: [BoolStep]
    boolsteps = map (\((_, e1), (_, e2)) -> (equivalenceCheck e1 e2, e2)) $ zip (("", e):steps) steps


-- Check if a list of laws are all true
checkLaws :: [String] -> Bool
checkLaws laws = and lawsTruth
  where
    laws' = map parseLaw laws
    lawsTruth = [equivalenceCheck lhs rhs| (Law _ (lhs, rhs)) <- laws']    


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

matchE (Bin op1 e1 e2) (Bin op2 e3 e4) | op1 == op2 
 = case (matchE e1 e3, matchE e2 e4) of
    (Just s1, Just s2) -> combineTwoSubsts s1 s2
    _ -> Nothing

-- matchE (Bin op1 e1 e2) (Bin op2 e3 e4) | op1 == op2 
--  = case tryMatch e1 e2 e3 e4 of -- And and Or operators are commutative, so check for reversed match
--           Nothing -> if op1 == And || op1 == Or then tryMatch e1 e2 e4 e3 else Nothing
--           res -> res
--    where 
--      tryMatch e1' e2' e3' e4'= do
--           s1 <- matchE e1' e3'
--           s2 <- matchE e2' e4'
--           combineTwoSubsts s1 s2 

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