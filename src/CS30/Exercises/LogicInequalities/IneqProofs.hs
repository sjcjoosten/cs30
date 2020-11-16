module CS30.Exercises.LogicInequalities.IneqProofs where
import           Data.Maybe
import           CS30.Exercises.LogicInequalities.IneqParser

{- laws to be used in our proofs -}
lawList :: [String]
lawList = [ "Exponentiation: a ^ 0 = 1"
          , "Exponentiation: a ^ 1 = a"
          , "Additive Identity: a + 0 = a"
          , "Additive Identity: 0 + a = a"
          , "Additive Inverse: a - a = 0"
          , "Additive Inverse: a + (- a) = 0"
          , "Subtraction by Addition: a + (- b) = a - b"
          , "Multiplicative Identity: a * 1 = a"
          , "Multiplicative Identity: 1 * a = a"
          , "Multiplication Times 0: 0 * a = 0"
          , "Multiplication Times 0: a * 0 = 0"
          , "Dividing Zero: 0 / a = 0"
          , "Distributive Law: a - (b + c) = a - b - c"
          , "Distributive Law: a - (b - c) = a - b + c"
          , "Additive Association: a + (b + c) = (a + b) + c"
          , "Multiplicative Association: a * (b * c) = (a * b) * c"
          , "Distributive Left: a * (c + d) = a * c + a * d"
          , "Distributive Right: (a + b) * c = a * c + b * c"
          , "Distributive Left: a * (c - d) = a * c - a * d"
          , "Distributive Right: (a - b) * c = a * c - b * c"
          ]

-- our list of laws
lawBois :: [Law]
lawBois = stringsToLaws lawList

-- parent function for generating a proof
-- e1 ≥ e2; flip them before generating proof if this isn't the case
makeProof :: Expr -> Expr -> Integer -> Proof
makeProof e1 e2 n = case evaluate e1 e2 of
                    Nothing -> makeProof e2 e1 n
                    Just (ineq,a) -> getDerivationLengthN lawBois ineq e1 e2 n a

-- given a list of laws, inequality symbol, two expressions, and the max length of proof, generate a proof
getDerivationLengthN :: [Law] -> Ineq -> Expr -> Expr -> Integer -> Integer -> Proof
getDerivationLengthN laws ineq e1 e2 i a = Proof (e1,ineq,e2) a (firstThing : (multiSteps e1 e2 i))
  where 
        -- get proof steps for first Expr
        multiSteps e1' e2' i' | i' <= 0 = (lastThing e1' e2')
                              | otherwise = case [ (lawName law, (res,ineq,e2'))
                                                 | law <- laws
                                                 , res <- getStep2 laws (lawEq law) ineq e1'
                                                 , res /= e1
                                                 ] of
                                             [] -> (addMore e1' e2' i')
                                             ((nm,(e1'',q,_)):_) -> (Double (nm,(e1'',q,e2'))) : multiSteps e1'' e2' (i' - 1)
        -- get proof steps for second Expr
        addMore e1' e2' i'    | i' <= 0 = (lastThing e1' e2')
                              | otherwise = case [ (lawName law, (e1',ineq,res))
                                                 | law <- laws
                                                 , res <- getStep2 laws (lawEq law) ineq e2'
                                                 , res /= e2
                                                 ] of
                                             [] -> (lastThing e1' e2')
                                             ((nm,(_,q,e2'')):_) -> (Double (nm,(e1',q,e2''))) : addMore e1' e2'' (i' - 1)
        -- first proof step, for clarity to the user
        firstThing = Double ("Starting Inequality", (e1,ineq,e2))
        -- last 2 proof steps
        -- sub in assumed value of n, then calculate
        lastThing e1' e2' = let (one,two) = sub (e1',e2') "n" a
                                one' = case compute one of
                                        (Just uno) -> uno
                                        Nothing -> undefined -- hopefully never reach this
                                two' = case compute two of
                                        (Just dos) -> dos
                                        Nothing -> undefined -- hopefully never reach this
                            in [Double ("Assumption",(one,ineq,two)), Double ("Calculation",(Const one',ineq,Const two'))]


-- used for subproofs of a single Expr (don't show up in final proof)
getSingleDerivation :: [Law] -> Ineq -> Expr -> Integer -> OldProof
getSingleDerivation laws ineq e1 i = OldProof e1 (multiSteps e1 i) 
  where multiSteps e1' i'     | i' <= 0 = []
                              | otherwise = case [ (lawName law, res)
                                                 | law <- laws
                                                 , res <- getStep2 laws (lawEq law) ineq e1'
                                                 , res /= e1
                                                 ] of
                                             [] -> []
                                             ((nm,e1''):_) -> (Single (nm,e1'')) : multiSteps e1'' (i' - 1)

-- try out laws on a single Expr
getStep :: Equation -> Expr -> [Expr]
getStep (lhs, rhs) expr
  = case matchE lhs expr of
      Nothing -> recurse expr
      Just subst -> [apply subst rhs]
  where recurse (Var _) = []
        recurse (Const _) = []
        recurse (Op o [e1,e2])
          = [Op o [e1', e2] | e1' <- getStep (lhs,rhs) e1] ++
            [Op o [e1, e2'] | e2' <- getStep (lhs,rhs) e2]
        recurse (Op o [e1])
          = [Op o [e1'] | e1' <- getStep (lhs,rhs) e1]
        recurse (Op _ _) = [] -- satisfy the compiler

-- try out laws on multiple Exprs, with result dependent on type of inequality
getStep2 :: [Law] -> (Expr, Expr) -> Ineq -> Expr -> [Expr]
getStep2 laws (lhs, rhs) ineq e
  = case matchE lhs e of
      Nothing -> recurse e
      Just subst -> [apply subst rhs]
  where recurse (Var _) = []
        recurse (Const _) = []
        recurse (Op o [e1,e2]) 
          = [Op o [e1', e2] | e1' <- getStep (lhs,rhs) e1, snd (checkFunc o) e2] ++
            [Op o [e1, e2'] | e2' <- getStep (lhs,rhs) e2, fst (checkFunc o) e1]
        recurse (Op o [e1])
          = [Op o [e1'] | e1' <- getStep (lhs,rhs) e1]
        recurse (Op _ _) = [] -- satisfy the compiler

        --"Multiplication for x > 0: y > z \\Rightarrow x * y > x * z"
        isPositive e1 = provesGEQ 1 (getSingleDerivation laws ineq e1 5)
        isNonNeg e1   = provesGEQ 0 (getSingleDerivation laws ineq e1 5)
        provesGEQ b (OldProof e1 steps) = go b e1 steps
              where go b' (Const n) _ | n >= b'     = True
                                     | otherwise = False
                    go b' _ ((Single (_ , e2)):ss) = go b' e2 ss --b-1 with strict inequalities
                    go _ _ [] = False
                    go _ _ _ = False

        -- see if this is a valid Opr to apply, based on inequality
        checkFunc Multiplication = case ineq of 
                                    EqEq  -> (const True, const True)
                                    GThan -> (isPositive, isPositive)
                                    GEq   -> (isNonNeg, isNonNeg)
        checkFunc Addition       = (const True, const True)
        checkFunc Division       = (const False, snd (checkFunc Multiplication))
        checkFunc Subtraction    = (const False, const True)
        checkFunc Exponentiation = (fst (checkFunc Multiplication), const True)
        checkFunc _              = (const False, const False) -- singleton Oprs; shouldn't work

type Substitution = [(String, Expr)]
matchE :: Expr -> Expr -> Maybe Substitution
matchE (Var nm) expr = Just [(nm,expr)]
matchE (Const i) (Const j) | i == j = Just []
matchE (Const _) _ = Nothing
matchE (Op o1 [e1,e2]) (Op o2 [e3,e4]) | o1 == o2
 = case (matchE e1 e3, matchE e2 e4) of
    (Just s1, Just s2) -> combineTwoSubsts s1 s2
    _                  -> Nothing
matchE (Op o1 [e1]) (Op o2 [e2]) | o1 == o2
 = matchE e1 e2
matchE (Op _ _) _ = Nothing

combineTwoSubsts :: Substitution -> Substitution -> Maybe Substitution
combineTwoSubsts s1 s2
  = case and [v1 == v2 | (nm1,v1) <- s1, (nm2,v2) <- s2, nm1 == nm2] of
     True -> Just (s1 ++ s2)
     False -> Nothing

apply :: Substitution -> Expr -> Expr
apply subst (Var nm) = lookupInSubst nm subst
apply _ (Const i) = Const i
apply subst (Op o [e]) = Op o [apply subst e]
apply subst (Op o [e1,e2]) = Op o [(apply subst e1),(apply subst e2)]
apply _ (Op _ _) = undefined

lookupInSubst :: String -> [(String, p)] -> p
lookupInSubst nm ((nm',v):rm)
 | nm == nm' = v
 | otherwise = lookupInSubst nm rm
lookupInSubst _ [] = error "Substitution was not complete, or free variables existed in the rhs of some equality"

-- only really supports monotonically increasing functions of one variable
-- i realize those are some pretty narrow restrictions
evaluate :: Expr -> Expr -> Maybe (Ineq,Integer)
evaluate one two = 
  let s = seeWhatWorks one two
  in findIneq (findSmallest s) s

-- compute both functions for values between [0..100]
seeWhatWorks :: Expr -> Expr -> [(Maybe Integer, Maybe Integer, Integer)]
seeWhatWorks left right = [ (leftVal,rightVal,tryNum)
                          | tryNum <- [0..100]
                          , let (l,r) = sub (left,right) "n" tryNum
                          , let leftVal = compute l
                          , let rightVal = compute r
                          , isJust leftVal && isJust rightVal
                          , leftVal >= rightVal]

-- find number of values where e2 > e1
findSmallest :: [(Maybe Integer, Maybe Integer, Integer)] -> Maybe Integer
findSmallest [] = Nothing
findSmallest l = case last l of
                (_,_,100) -> Just (last (zipWith (\(_,_,x) y -> x - y) l [0..100]))
                (_,_,_) -> Nothing

-- tries to return smallest value for which e1 ≥ e2
findIneq :: Maybe Integer -> [(Maybe Integer,Maybe Integer,Integer)] -> Maybe (Ineq,Integer)
findIneq Nothing _ = Nothing
findIneq _ [] = Nothing
findIneq (Just v) ((l,r,n):xs) | n == v = case (l>r) of
                                            True -> Just (GThan,n)
                                            False -> Just (GEq,n)
                               | otherwise = findIneq (Just (v+1)) xs

-- substitute a certain Var for a certain Const
sub :: (Expr,Expr) -> String -> Integer -> (Expr,Expr)
sub (l,r) v n = (go l v n, go r v n)
  where go expr val num
          = case expr of
              Const someNum -> Const someNum
              Var thisVal -> case (thisVal == val) of
                              True -> Const n
                              False -> Var thisVal -- sneaky variable sneaks in, let it stay
              Op o [e1] -> Op o [go e1 val num]
              Op o [e1,e2] -> Op o [go e1 val num, go e2 val num]
              Op _ _ -> undefined

-- Does only integer math
compute :: Expr -> Maybe Integer
compute (Const c) = Just c
compute (Var _) = Just 1 -- ?????? what to do about sneaky variables
compute (Op o [e1]) = case (compute e1, o) of
                          (Nothing,_) -> Nothing
                          (Just eval1,Factorial) -> evalFac eval1
                          (Just eval1, Negate) -> Just (-1 * eval1)
                          (_,_) -> Nothing
compute (Op o [e1,e2]) = case (compute e1, compute e2, o) of
                          (Nothing,_,_) -> Nothing
                          (Just _, Nothing,_) -> Nothing
                          (Just eval1, Just eval2, Addition)-> Just (eval1 + eval2)
                          (Just eval1, Just eval2, Subtraction) -> Just (eval1 - eval2)
                          (Just eval1, Just eval2, Multiplication) -> Just (eval1 * eval2)
                          (Just eval1, Just eval2, Division) -> case eval2 of
                                                                      0 -> Nothing
                                                                      _ -> Just (eval1 `div` eval2)
                          (Just eval1, Just eval2, Exponentiation) -> Just (eval1 ^ eval2)
                          (_,_,_) -> Nothing
compute _ = Nothing

-- Factorial function
evalFac :: Integer -> Maybe Integer
evalFac n = case (n>=0) of
              False -> Nothing
              True -> Just (realFac n)
  where realFac 0 = 1
        realFac x = x * realFac (x - 1)