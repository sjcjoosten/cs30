{-# OPTIONS_GHC -Wall #-}
module CS30.Exercises.ModularArithmetic.ModuloProof where

import CS30.Exercises.ModularArithmetic.ModuloParser

------------------------------------------Implicit Laws----------------------------------------

-- modulo_laws :: [Law]
-- modulo_laws = map (parseLaw False Congruent)
--               [
--                "Law2 : p \\equiv_p 0 (mod p)",
--                "Law1 : x ^ (p-1) \\equiv_p 1 (mod p)"
--               ]

_arithmetic_laws :: [Law]
_arithmetic_laws = map (parseLaw False)
                  [
                     "Law1 : x \\cdot 0 = 0"
                    ,"Law2 : x + 0 = x"
                    ,"Law3 : x \\cdot 1 = x"
                    ,"Law4 : x \\cdot -1 = -x"
                    ,"Law5 : x ^ 0 = 1"
                    ,"Law6 : 1 ^ x = 1"
                    ,"Law7 : x \\cdot (y + z) = x \\cdot y + x \\cdot z"
                    ,"Law8 : x \\cdot (y - z) = x \\cdot y - x \\cdot z"
                    ,"Law9 : (x + y) \\cdot z = x \\cdot z + y \\cdot z"
                    ,"Law10 : (x - y) \\cdot z = x \\cdot z - y \\cdot z"
                    ,"Law11 : x ^ (y + z) = x ^ y \\cdot x ^ z"
                    ,"Law12 : (x \\cdot y) ^ z = x ^ z \\cdot y ^ z"
                    -- Least priority
                    ,"Law13 : x + y = y + x"
                    ,"Law14 : x \\cdot y = y \\cdot x"
                    ,"Law15 : (x + y) + z = x + (y + z)"
                    ,"Law16 : (x \\cdot y) \\cdot z = x \\cdot (y \\cdot z)"
                  ]
                  
-------------------------------------Proofs----------------------------------------

getDerivation :: Int -> [Law] -> Expression -> Proof
getDerivation steps laws e
 = Proof e (multiSteps steps e)
 where multiSteps 0 _ = [] -- [proofSteps]
       multiSteps steps' e'
        = case [ (lawName law, res) -- proofStep
               | law <- laws -- Law
               , res <- getStep (lawEq law) e' -- getStep
               ] of
                   [] -> []
                   ((nm,e''):_) -> (nm,e'') : multiSteps (steps'-1) e''

-- would go into rewrite function
-- Law1 : a = b (mod p) implies a + c = b + c (mod p)
-- Law2 : a = b (mod p) implies c + a = c + b (mod p)
-- Law3 : a = b (mod p) implies a \\cdot c = b \\cdot c (mod p)
-- Law4 : a = b (mod p) implies c \\cdot a = c \\cdot b (mod p)
-- Law5 : a = b (mod p) implies a ^ c = b ^ c (mod p)
getStep :: Equation -> Expression -> [Expression]
getStep (lhs, rhs) expr
  = case matchE lhs expr of
      Nothing -> recurse expr
      Just subst -> [apply subst rhs]
  where recurse (Var _) = []
        recurse (Con _) = []
        recurse (Fixed _) = []
        recurse (BinOp op e1 e2)
          = [BinOp op e1' e2 | e1' <- getStep (lhs,rhs) e1] ++
            [BinOp op e1 e2' | e2' <- getStep (lhs,rhs) e2]
        recurse (UnOp op e1)
          = [UnOp op e1' | e1' <- getStep (lhs,rhs) e1]
        recurse (ExpressionError) = []

matchE :: Expression -> Expression -> Maybe Substitution
matchE (Var nm) expr = Just [([nm],expr)]
matchE (Con i) (Con j) | i == j = Just []
matchE (Con _) _ = Nothing
matchE (Fixed i) (Fixed j) | i == j = Just [] -- TODO : Is this similar to Var or Con?
matchE (Fixed _) _ = Nothing -- TODO : Is this similar to Var or Con?
matchE (BinOp op1 e1 e2) (BinOp op2 e3 e4) | op1 == op2
 = case (matchE e1 e3, matchE e2 e4) of
    (Just s1, Just s2) -> combineTwoSubsts s1 s2
    _ -> Nothing
matchE (BinOp _ _ _) _ = Nothing
matchE (UnOp Neg e1) (UnOp Neg e2) = matchE e1 e2
matchE (UnOp _ _) _ = Nothing
matchE ExpressionError _ = Nothing

-- For common var, the val should match
combineTwoSubsts :: Substitution -> Substitution -> Maybe Substitution
combineTwoSubsts s1 s2
  = case and [v1 == v2 | (nm1,v1) <- s1, (nm2,v2) <- s2, nm1 == nm2] of
     True -> Just (s1 ++ s2)
     False -> Nothing

apply :: Substitution -> Expression -> Expression
apply subst (Var nm) = lookupInSubst [nm] subst
apply _ (Con i) = Con i
apply _ (Fixed i) = Fixed i
apply subst (UnOp op e) = UnOp op (apply subst e)
apply subst (BinOp op e1 e2) = BinOp op (apply subst e1) (apply subst e2)
apply _ ExpressionError = ExpressionError

lookupInSubst :: String -> [(String, Expression)] -> Expression
lookupInSubst nm ((nm',v):rm)
 | nm == nm' = v
 | otherwise = lookupInSubst nm rm
lookupInSubst _ [] = error "Substitution was not complete, or free variables existed in the rhs of some equality"

-------------------------------------Evaluation----------------------------------------

evalExpression :: Substitution -> Int -> Expression -> Maybe Int
evalExpression s p = evalExpression' p . evalApply s

evalExpression' :: Int -> Expression -> Maybe Int
evalExpression' p (Con i) = modulo p (Just i)
evalExpression' p (UnOp op e) = modulo p (fnOp op (evalExpression' p e) Nothing)
evalExpression' p (BinOp op e1 e2) = modulo p (fnOp op (evalExpression' p e1) (evalExpression' p e2))
evalExpression' _ _ = Nothing

-- TODO : Handle 0 ^ 0
fnOp :: Op -> Maybe Int -> Maybe Int -> Maybe Int
fnOp Neg (Just x) _ = Just (-x)
fnOp op (Just i1) (Just i2) = Just (opVal i1 i2)
                              where opVal = case op of {Pow -> (^); Mul -> (*); Sub -> (-); Add -> (+); _ -> (+)}
fnOp _ _ _ = Nothing

modulo :: Int -> Maybe Int -> Maybe Int
modulo p (Just i) = Just (i `mod` p)
modulo _ _ = Nothing

evalApply :: Substitution -> Expression -> Expression
evalApply subst (Var nm) = lookupInSubst [nm] subst
evalApply _ (Con i) = Con i
evalApply subst (Fixed nm) = lookupInSubst [nm] subst
evalApply subst (UnOp op e) = UnOp op (evalApply subst e)
evalApply subst (BinOp op e1 e2) = BinOp op (evalApply subst e1) (evalApply subst e2)
evalApply _ _ = ExpressionError

getVariables :: Expression -> [Char]
getVariables (Var v) = [v]
getVariables (Fixed f) = [f]
getVariables (Con _) = []
getVariables (UnOp _ e) = getVariables e
getVariables (BinOp _ e1 e2) = getVariables e1 ++ getVariables e2
getVariables _ = []

-------------------------------------Tests----------------------------------------

_exp :: Expression
-- _exp = parseExpression True "(a \\cdot 3) ^ b \\cdot (c + d)"
_exp = parseExpression True "(a + b) \\cdot (c + d)"

_subst :: Substitution
_subst = [("a", Con 3), ("b", Con 2), ("c", Con 1), ("d", Con 4)]

_eval :: Maybe Int
_eval = evalExpression _subst 10 _exp

_given :: Law
_given = parseLaw True "given1 : a = b + c"

_prf :: Proof
_prf = getDerivation 5 (_given:_arithmetic_laws) _exp

--------------------------------

testLaw :: Law -> Bool
testLaw (Law _ (lhs, rhs)) = and [ evalExpression subst 101 lhs == evalExpression subst 101 rhs 
                                  | x <- [2,3,4],
                                    y <- [2,3,4],
                                    z <- [2,3,4],
                                  let subst = [("x", Con x), ("y", Con y), ("z", Con z)]]
       

