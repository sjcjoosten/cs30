module CS30.Exercises.LogicInequalities.IneqProofs where

import           CS30.Exercises.Data
import           Data.Maybe
import           CS30.Exercises.LogicInequalities.IneqParser

{- some sample expressions -}
expr1 :: Expr
expr1 = Op Addition [Op Multiplication [Const 3,Var "n"], Const 6]
expr2 :: Expr
expr2 = Op Exponentiation [Var "n", Const 3]
expr3 :: Expr
expr3 = Op Factorial [Var "n"]
expr4 :: Expr
expr4 = Op Exponentiation [Op Addition [Var "n", Const 4], Const 2]


{- laws to be used in our proofs -}

-- some of these are duplicated, ex: a * 0 = 0 and 0 * a = 0
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
          -- , "Exponentiation: a ^ 2 = a * a"
          ]

lawBois = stringsToLaw lawList

getDerivationLengthN :: [Law] -> Ineq -> Expr -> Expr -> Integer -> Proof
getDerivationLengthN laws ineq e1 e2 i = Proof e1 (multiSteps e1 e2 i) 
  where multiSteps e1' e2' i' | i' <= 0 = []
                              | otherwise = case [ (lawName law, res)
                                                 | law <- laws
                                                 , res <- getStep2 laws (lawEq law) ineq e1'
                                                 , res /= e1
                                                 ] of
                                             [] -> case doMath e1' of
                                                    ((nm,e1''):_) -> (Single (nm,e1'')) : multiSteps e1'' e2' (i' - 1)
                                                    [] -> (firstThing e1' e2') : (addMore e1' e2' i')
                                             ((nm,e1''):_) -> (Single (nm,e1'')) : multiSteps e1'' e2' (i' - 1)
        addMore e1' e2' i'    | i' <= 0 = []
                              | otherwise = case [ (lawName law, (e1',ineq,res))
                                                 | law <- laws
                                                 , res <- getStep2 laws (lawEq law) ineq e2'
                                                 , res /= e2
                                                 ] of
                                             [] -> case doMath2 (e1',ineq,e2') of
                                                    ((nm,(e1',q,e2'')):_) -> (Double (nm,(e1',q,e2''))) : addMore e1' e2'' (i' - 1)
                                                    [] -> []
                                             ((nm,(e1',q,e2'')):_) -> (Double (nm,(e1',q,e2''))) : addMore e1' e2'' (i' - 1)
        firstThing e1' e2' = Double ("Assumption",(e1',ineq,e2'))

getSingleDerivation :: [Law] -> Ineq -> Expr -> Integer -> Proof
getSingleDerivation laws ineq e1 i = Proof e1 (multiSteps e1 i) 
  where multiSteps e1' i'     | i' <= 0 = []
                              | otherwise = case [ (lawName law, res)
                                                 | law <- laws
                                                 , res <- getStep2 laws (lawEq law) ineq e1'
                                                 , res /= e1
                                                 ] of
                                             [] -> []
                                             ((nm,e1''):_) -> (Single (nm,e1'')) : multiSteps e1'' (i' - 1)

-- does some specific pattern matching to see if certain expressions can be simplified
doMath :: Expr -> [(String,Expr)]
doMath (Op o [e1,e2]) = case (e1,e2) of
                        (Const a, Const b) 
                            -> case o of
                                Multiplication -> [("Do Math", Const (a * b))]
                                Addition -> [("Do Math", Const (a + b))]
                                Subtraction -> [("Do Math", Const (a - b))]
                                Division -> [("Do Math", Const (a `div` b))]
                                Exponentiation -> [("Do Math", Const (a ^ b))]
                                _ -> []
                        (Op Multiplication [Const a, Var b],
                         Op Multiplication [Const c, Var d]) 
                            -> case o of
                                Addition -> [("Do Math", Op Multiplication [Const (a + c), Var b])]
                                Subtraction -> [("Do Math", Op Multiplication [Const (a - c), Var b])]
                                Multiplication -> [("Do Math", Op Multiplication [Const (a * c), Op Exponentiation [Var b, Const 2]])]
                                Division -> [("Do Math", Const (a `div` c))]
                                _ -> []
                        (Op Multiplication [Var b, Const a],
                         Op Multiplication [Const c, Var d]) 
                            -> case o of
                                Addition -> [("Do Math", Op Multiplication [Const (a + c), Var b])]
                                Subtraction -> [("Do Math", Op Multiplication [Const (a - c), Var b])]
                                Multiplication -> [("Do Math", Op Multiplication [Const (a * c), Op Exponentiation [Var b, Const 2]])]
                                Division -> [("Do Math", Const (a `div` c))]
                                _ -> []
                        (Op Multiplication [Const a, Var b],
                         Op Multiplication [Var d, Const c]) 
                            -> case o of
                                Addition -> [("Do Math", Op Multiplication [Const (a + c), Var b])]
                                Subtraction -> [("Do Math", Op Multiplication [Const (a - c), Var b])]
                                Multiplication -> [("Do Math", Op Multiplication [Const (a * c), Op Exponentiation [Var b, Const 2]])]
                                Division -> [("Do Math", Const (a `div` c))]
                                _ -> []
                        (Op Multiplication [Var b, Const a],
                         Op Multiplication [Var d, Const c]) 
                            -> case o of
                                Addition -> [("Do Math", Op Multiplication [Const (a + c), Var b])]
                                Subtraction -> [("Do Math", Op Multiplication [Const (a - c), Var b])]
                                Multiplication -> [("Do Math", Op Multiplication [Const (a * c), Op Exponentiation [Var b, Const 2]])]
                                Division -> [("Do Math", Const (a `div` c))]
                                _ -> []
                        (Op Multiplication [Var b, Const a], Const c) 
                            -> case o of
                                Multiplication -> [("Do Math", Op Multiplication [Const (a * c), Var b])]
                                Division -> [("Do Math", Op Multiplication [Const (a `div` c),Var b])]
                                _ -> []
                        (Op Multiplication [Const a, Var b], Const c) 
                            -> case o of
                                Multiplication -> [("Do Math", Op Multiplication [Const (a * c), Var b])]
                                Division -> [("Do Math", Op Multiplication [Const (a `div` c),Var b])]
                                _ -> []
                        (Const c, Op Multiplication [Var b, Const a]) 
                            -> case o of
                                Multiplication -> [("Do Math", Op Multiplication [Const (a * c), Var b])]
                                Division -> [("Do Math", Op Multiplication [Const (c `div` a),Var b])]
                                _ -> []
                        (Const c, Op Multiplication [Const a, Var b])
                            -> case o of
                                Multiplication -> [("Do Math", Op Multiplication [Const (a * c), Var b])]
                                Division -> [("Do Math", Op Multiplication [Const (c `div` a),Var b])]
                                _ -> []
                        (_,_) -> case doMath e1 of
                                  [("Do Math",e1')] -> [("Do Math", Op o [e1',e2])]
                                  [] -> case doMath e2 of
                                          [("Do Math",e2')] -> [("Do Math", Op o [e1,e2'])]
                                          [] -> []
doMath _ = []

doMath2 :: Inequality -> [(String,Inequality)]
doMath2 (e1,i,e2) = case doMath e1 of
                      [] -> []
                      ((a,b)):_ -> [(a,(e1,i,b))]

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
        provesGEQ b (Proof e1 steps) = go b e1 steps
              where go b' (Const n) _ | n >= b'     = True
                                     | otherwise = False
                    --go b' (Op Negation [e]) _ = False
                    go b' _ ((Single (_ , e2)):ss) = go b' e2 ss --b-1 with strict inequalities
                    go _ _ [] = False

        checkFunc Multiplication = case ineq of 
                                    EqEq  -> (const True, const True)
                                    GThan -> (isPositive, isPositive)
                                    GEq   -> (isNonNeg, isNonNeg)
        checkFunc Addition       = (const True, const True)
        checkFunc Division       = (const False, snd (checkFunc Multiplication))
        checkFunc Subtraction    = (const False, const True)
        -- add support for Exponentiation, Factorial, Negate

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
apply subst (Op _ _) = undefined

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

seeWhatWorks :: Expr -> Expr -> [(Maybe Integer, Maybe Integer, Integer)]
seeWhatWorks left right = [ (leftVal,rightVal,tryNum)
                          | tryNum <- [0..100]
                          , let (l,r) = sub (left,right) "n" tryNum
                          , let leftVal = compute l
                          , let rightVal = compute r
                          , isJust leftVal && isJust rightVal
                          , leftVal >= rightVal]

findSmallest :: [(Maybe Integer, Maybe Integer, Integer)] -> Maybe Integer
findSmallest l = case last l of
                (_,_,100) -> Just (last (zipWith (\(_,_,x) y -> x - y) l [0..100]))
                (_,_,_) -> Nothing

findIneq :: Maybe Integer -> [(Maybe Integer,Maybe Integer,Integer)] -> Maybe (Ineq,Integer)
findIneq Nothing _ = Nothing
findIneq _ [] = Nothing
findIneq (Just v) ((l,r,n):xs) | n == v = case (l>r) of
                                            True -> Just (GThan,n)
                                            False -> Just (GEq,n)
                               | otherwise = findIneq (Just (v+1)) xs

sub :: (Expr,Expr) -> String -> Integer -> (Expr,Expr)
sub (l,r) v n = (go l v n, go r v n)
  where go expr val num
          = case expr of
              Const someNum -> Const someNum
              Var thisVal -> case (thisVal == val) of
                              True -> Const n
                              False -> Var thisVal -- idk what to do about this particular line
              Op o [e1] -> Op o [go e1 val num]
              Op o [e1,e2] -> Op o [go e1 val num, go e2 val num]
              Op _ _ -> undefined

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
                          (Just _, Nothing,_) -> Nothing
                          (Just eval1, Just eval2, Addition)-> Just (eval1 + eval2)
                          (Just eval1, Just eval2, Subtraction) -> Just (eval1 - eval2)
                          (Just eval1, Just eval2, Multiplication) -> Just (eval1 * eval2)
                          (Just eval1, Just eval2, Division) -> case eval2 of
                                                                      0 -> Nothing
                                                                      _ -> Just (eval1 `div` eval2)
                          (Just eval1, Just eval2, Exponentiation) -> Just (eval1 ^ eval2)

-- (n + 6) - 3
-- 1. assume n > generateRandEx "c"
-- 2. assume 6 > generateRandEx "c"
-- 3. assume (n + 6) > generateRandEx "c"

-- Factorial function
evalFac :: Integer -> Maybe Integer
evalFac n = case (n>=0) of
              False -> Nothing
              True -> Just (realFac n)
  where realFac 0 = 1
        realFac x = x * realFac (x - 1)


generateRandIneq :: ChoiceTree Ineq
generateRandIneq = Branch [Node GThan, Node GEq]

-- return valid expressions, i.e. those that have a variable in them
-- just calling generateRandEx i<1 may or may not generate a valid expression
-- but calling generateRandEx i≥1 will filter out the invalid ones
generateRandEx :: Int -> String -> ChoiceTree Expr
generateRandEx i n | i < 1
 = Branch [ Branch [Node (Var varName) | varName <- [n]] -- add support for more variables
          , Branch [Node (Const val) | val <- [2..9]]
          ]
generateRandEx i n
 = filterTree $ Branch [do {e1 <- generateRandEx i' n
                       ;e2 <- generateRandEx (i - i' - 1) n
                       ;opr <- nodes $ availOps i
                       ;return (Op opr [e1,e2])
                       }
                       | i' <- [0..i-1]
                       ]
    where
      availOps j = case (j < 2) of
                    True -> [Addition,Subtraction,Multiplication,Exponentiation]
                    False -> [Addition,Multiplication]

filterTree :: ChoiceTree Expr -> ChoiceTree Expr
filterTree (Branch b@((Branch _):_)) = Branch $ filter isNonEmpty $ map filterTree b
filterTree (Branch n@((Node _):_)) =  Branch $ filter nodeHasVar n
filterTree n = n

isNonEmpty :: ChoiceTree Expr -> Bool
isNonEmpty (Node _) = True
isNonEmpty (Branch []) = False
isNonEmpty (Branch b) = or $ map isNonEmpty b

nodeHasVar :: ChoiceTree Expr -> Bool
nodeHasVar (Branch _) = False
nodeHasVar (Node n) = hasVar n
  where
    hasVar (Var _) = True
    hasVar (Const _) = False
    hasVar (Op _ [e1]) = hasVar e1
    hasVar (Op _ [e1,e2]) = hasVar e1 || hasVar e2
    hasVar (Op _ _) = False