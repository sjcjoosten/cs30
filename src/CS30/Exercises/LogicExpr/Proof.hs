{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.LogicExpr.Proof where
import CS30.Exercises.LogicExpr.Parser hiding (law)
import Data.Maybe ( fromJust )

type Substitution = [(VarName,LogicExpr)]
type VarName = Char

data Proof a = Proof LogicExpr [Step a] deriving (Show)

type Step a= (a, LogicExpr)

input_laws :: [String]
input_laws = [
    "Negation of Negation:¬(¬p)≡p"
    , "Idempotence:p∧p≡p"
    , "Operation with true,false:p∧true≡p"
    , "Operation with true,false:p∨true≡true"
    , "Idempotence:p∨p≡p"
    , "Implications as an OR:(p ⇒ q) ≡ ¬p ∨ q"    
    -- Easy challenge: only apply law when a negation before it; 
    -- Not sure if this makes proof more interesting, so commented out for now
    -- , "Implications as an OR:¬(p⇒q)≡¬(¬p∨q)"    
    , "Operation with true,false:p∨false≡p"
    , "Operation with true,false:p∧false≡false"
    , "DeMorgan's Law:¬(p∨q)≡¬p∧¬q"
    , "DeMorgan's Law:¬(p∧q)≡¬p∨¬q"
    , "Operation with Negation:p∧¬p≡false"
    , "Operation with Negation:p∨¬p≡true"

    -- Below are not in the given law list 
    , "Redundancy law: (p∨q)∧(p∨¬q)≡p"
    , "Redundancy law: p∨(¬p∧q)≡p∨q"
    , "Complement Law: ¬true≡false"
    , "Complement Law: ¬false≡true"
    
    -- Optional Distributivity    
    -- , "Distributivity:p∨(q∧r)≡(p∨q)∧(p∨r)"
    -- , "Distributivity:(q∧r)∨p≡(q∨p)∧(r∨p)"

    -- , "Associativity: (p∨q)∨r≡p∨(q∨r)"
    -- , "Associativity: p∨(q∨r)≡(p∨q)∨r"    
    -- , "Associativity: (p∧q)∧r≡p∧(q∧r)"
    -- , "Associativity: p∧(q∧r)≡(p∧q)∧r"
    -- Commutative laws give potentially verbose proof
    -- , "Commutativity: p∧q≡q∧p"
    -- , "Commutativity: p∨q≡q∨p"    
    ]

fake_laws :: [String]
fake_laws = [
    "FAKELAW: ¬(p ⇒ q) ≡ ¬p ⇒ ¬q"
    , "FAKELAW: p∨(q∧r)≡(p∨q)∧r"
    , "FAKELAW: (p∧q)∨r≡p∧(q∨r)"
    , "FAKELAW: ¬(p∧q)≡(p∨q)"
    , "FAKELAW: ¬(p∧q)≡(¬p∧¬q)"
    , "FAKELAW: ¬(p⇒q)≡(¬p⇒¬q)"
    , "FAKELAW: ¬(p⇒q)≡(q⇒p)"
    , "FAKELAW: ¬(p⇒q)≡(¬q⇒¬p)"
    , "FAKELAW: false⇒q≡¬q"
  ]

renameLaw :: (a -> b) -> Law a -> Law b
renameLaw f law = law {lawName=f (lawName law)}

-- try to generate a faulty proof for an expr
genFaultyProof :: LogicExpr -> Proof Bool
genFaultyProof = getWrongDerivation true_laws false_laws
  where 
    true_laws = map (renameLaw (const True) . parseLaw) input_laws
    false_laws = map (renameLaw (const False) . parseLaw) fake_laws

-- Check if a list of laws are all true
checkLaws :: [String] -> Bool
checkLaws = all checkLaw
checkLaw :: String -> Bool
checkLaw law = case parseLaw law of
                 (Law _ (lhs, rhs)) -> equivalenceCheck lhs rhs

-- Simlify an expr with given laws
simplify :: [String] -> String -> Proof LawName
simplify strings str = getDerivation laws e
  where
    laws = map parseLaw strings
    e = parseExpr str

getWrongDerivation :: [Law Bool] -> [Law Bool] -> LogicExpr -> Proof Bool
getWrongDerivation true_laws false_laws e
 = Proof e (multiSteps e 0)
 where multiSteps :: LogicExpr -> Int -> [Step Bool] 
       multiSteps e' stepNum
        = case [ (lawName law, res)
               | law <- if even stepNum then true_laws++false_laws else false_laws++true_laws -- true_laws and false_laws in alternatining order of appearance
               , res <- getStep (lawEq law) e'               
               , lawName law || not (equivalenceCheck e' res)
               ] of
            [] -> []
            (x:_xs) -> x:multiSteps (snd x) (stepNum+1)


getDerivation :: [Law a] -> LogicExpr -> Proof a
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

matchE Bin {} _ = Nothing
matchE (Neg e1) (Neg e2) = matchE e1 e2
matchE (Neg _) _ = Nothing

combineTwoSubsts :: Substitution -> Substitution -> Maybe Substitution
combineTwoSubsts s1 s2
  = if and [v1 == v2 | (nm1,v1) <- s1, (nm2,v2) <- s2, nm1 == nm2]
    then Just (s1 ++ s2)
    else Nothing

apply :: Substitution -> LogicExpr -> LogicExpr
apply subst (Var nm) = fromJust $ lookup nm subst
apply _ (Con i) = Con i
apply subst (Neg e) = Neg (apply subst e)
apply subst (Bin op e1 e2) = Bin op (apply subst e1) (apply subst e2)