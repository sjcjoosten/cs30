module CS30.Exercises.LogicInequalities.IneqProofs where

import           CS30.Exercises.Data
import           Data.Maybe
import           CS30.Exercises.LogicInequalities.IneqParser

getDerivation :: [Law] -> Ineq -> Expr -> Proof
getDerivation laws ineq e = Proof e (multiSteps e)
  where multiSteps e'
          = case [ (lawName law, res)
                 | law <- laws
                 , res <- getStep2 laws (lawEq law) ineq e'
                 ] of
            [] -> []
            ((nm,e''):_) -> (nm,e'') : multiSteps e''

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

        --"Multiplication for x > 0: y > z \\Rightarrow x * y > x * z"
        isPositive e1 = provesGEQ 1 (getDerivation laws ineq e1)
        isNonNeg e1   = provesGEQ 0 (getDerivation laws ineq e1)
        provesGEQ b (Proof e1 steps) = go b e1 steps
              where go b' (Const n) _ | n >= b'     = True
                                     | otherwise = False
                    --go b' (Op Negation [e]) _ = False
                    go b' _ ((_ , e2):ss) = go b' e2 ss --b-1 with strict inequalities
                    go _ _ [] = False

        checkFunc Multiplication = case ineq of 
                                    EqEq  -> (const True, const True)
                                    GThan -> (isPositive, isPositive)
                                    GEq   -> (isNonNeg, isNonNeg)
        checkFunc Addition       = (const True, const True)
        checkFunc Division       = (const False, snd (checkFunc Multiplication))
        checkFunc Subtraction    = (const False, const True)

-- I think we will need to change this function
type Substitution = [(String, Expr)]
matchE :: Expr -> Expr -> Maybe Substitution
matchE (Var nm) expr = Just [(nm,expr)]
matchE (Const i) (Const j) | i == j = Just []
matchE (Const _) _ = Nothing
matchE (Op o1 [e1,e2]) (Op o2 [e3,e4]) | o1 == o2
 = case (matchE e1 e3, matchE e2 e4) of
    (Just s1, Just s2) -> combineTwoSubsts s1 s2
    _ -> Nothing
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

lookupInSubst :: String -> [(String, p)] -> p
lookupInSubst nm ((nm',v):rm)
 | nm == nm' = v
 | otherwise = lookupInSubst nm rm
lookupInSubst _ [] = error "Substitution was not complete, or free variables existed in the rhs of some equality"

-- only supports monotonically increasing functions of one variable
-- i realize those are some pretty narrow restrictions
-- can expand the function if necessary
-- or we can just carefully generate expressions that fit these params
evaluate :: Expr -> Expr -> Maybe (Ineq,Integer)
evaluate one two = 
  let s = seeWhatWorks one two
  in findIneq (findSmallest s) s
  where
    seeWhatWorks left right = [ (leftVal,rightVal,tryNum)
                              | tryNum <- [0..100]
                              , let (l,r) = sub (left,right) "n" tryNum
                              , let leftVal = compute l
                              , let rightVal = compute r
                              , isJust leftVal && isJust rightVal
                              , leftVal >= rightVal]
    findSmallest l = case last l of
                    (_,_,100) -> Just (last (zipWith (\(_,_,x) y -> x - y) l [0..100]) + 1)
                    (_,_,_) -> Nothing

expr1 :: Expr
expr1 = Op Addition [Op Multiplication [Const 3,Var "n"], Const 6]
expr2 :: Expr
expr2 = Op Exponentiation [Var "n", Const 2]
expr3 :: Expr
expr3 = Op Factorial [Var "n"]

findIneq :: Maybe Integer -> [(Maybe Integer,Maybe Integer,Integer)] -> Maybe (Ineq,Integer)
findIneq Nothing _ = Nothing
findIneq _ [] = Nothing
findIneq (Just v) ((l,r,n):xs) | n == v = case (l>r) of
                                            True -> Just (GThan,n)
                                            False -> Just (GEq,n)
                               | n == v + 1 = case (l>r) of
                                                True -> Just (GThan,n)
                                                False -> Just (GEq,n)
                               | otherwise = findIneq (Just v) xs

sub :: (Expr,Expr) -> String -> Integer -> (Expr,Expr)
sub (l,r) v n = (go l v n, go r v n)
  where go expr val num
          = case expr of
              Const someNum -> Const someNum
              Var v -> case (v == val) of
                        True -> Const n
                        False -> Var v -- idk what to do about this particular line
              Op o [e1] -> Op o [go e1 val num]
              Op o [e1,e2] -> Op o [go e1 val num, go e2 val num]

-- Does only integer math for now
-- Would need to alter our datatype to do work with rationals/floats
compute :: Expr -> Maybe Integer
compute (Const c) = Just c
compute (Var _) = Just 1 -- ?????? what to do about sneaky variables
compute (Op o [e1]) = case (compute e1, o) of
                          (Nothing,_) -> Nothing
                          (Just eval1,Factorial) -> evalFac eval1
                          (Just eval1, Negate) -> Just (-1 * eval1)
compute (Op o [e1,e2]) = case (compute e1, compute e2, o) of
                          (Nothing,_,_) -> Nothing
                          (Just eval1, Nothing,_) -> Nothing
                          (Just eval1, Just eval2, Addition)-> Just (eval1 + eval2)
                          (Just eval1, Just eval2, Subtraction) -> Just (eval1 - eval2)
                          (Just eval1, Just eval2, Multiplication) -> Just (eval1 * eval2)
                          (Just eval1, Just eval2, Division) -> case eval2 of
                                                                      0 -> Nothing
                                                                      _ -> Just (eval1 `div` eval2)
                          (Just eval1, Just eval2, Exponentiation) -> Just (eval1 ^ eval2)

-- Factorial function
evalFac :: Integer -> Maybe Integer
evalFac n = case (n>=0) of
              False -> Nothing
              True -> Just (realFac n)
  where realFac 0 = 1
        realFac x = x * realFac (x - 1)


-- return valid expressions, i.e. those that have a variable in them
-- just calling generateRandEx i<1 may or may not generate a valid expression
-- but calling generateRandEx i≥1 will filter out the invalid ones
generateRandEx :: Int -> ChoiceTree Expr
generateRandEx i | i < 1
 = Branch [ Branch [Node (Var varName) | varName <- ["n"]] -- add support for more variables
          , Branch [Node (Const val) | val <- [2..10]]
          ]
generateRandEx i
 = filterTree $ Branch [do {e1 <- generateRandEx i'
                       ;e2 <- generateRandEx (i - i' - 1)
                       ;opr <- nodes [Addition,Subtraction,Multiplication,Division,Exponentiation]
                       ;return (Op opr [e1,e2])
                       }
                       | i' <- [0..i-1]
                       ]

filterTree :: ChoiceTree Expr -> ChoiceTree Expr
filterTree (Branch b@((Branch _):_)) = Branch $ filter isNonEmpty $ map filterTree b
filterTree (Branch n@((Node _):_)) =  Branch $ filter nodeHasVar n
filterTree n = n

isNonEmpty :: ChoiceTree Expr -> Bool
isNonEmpty (Node _) = True
isNonEmpty (Branch []) = False
isNonEmpty (Branch b) = or $ map isNonEmpty b

nodeHasVar :: ChoiceTree Expr -> Bool
nodeHasVar (Branch b) = False
nodeHasVar (Node n) = hasVar n
  where
    hasVar (Var _) = True
    hasVar (Const _) = False
    hasVar (Op _ [e1]) = hasVar e1
    hasVar (Op _ [e1,e2]) = hasVar e1 || hasVar e2
    hasVar (Op _ _) = False