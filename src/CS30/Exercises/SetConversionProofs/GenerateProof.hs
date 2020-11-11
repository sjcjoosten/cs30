module CS30.Exercises.SetConversionProofs.GenerateProof  where
import CS30.Exercises.Data
import CS30.Exercises.SetConversionProofs.SetExprParser
import CS30.Exercises.SetConversionProofs.LawParser
import Data.List --( intersect, \\ )


data Proof = Proof SetExpr [(String, SetExpr)] deriving Show

-- fxn for generating a random expression, using \cap, \cup, and \setminus operators (as specified in the assignment sheet)
-- for testing
generateRandEx :: Int -> ChoiceTree SetExpr
generateRandEx i | i < 1
 = Branch [ Branch [Node (Var varName) | varName <- ["A","B","C"]] -- should I have options like (Cap (Var "A") (Var "A") ? (do this and document in comments, as a creative)
          ]
generateRandEx i
 = do { i' <- nodes [0..i-1]
        ;e1 <- generateRandEx i'
        ;e2 <- generateRandEx (i - i' - 1)
        ;opr <- nodes [Cap, Cup, SetMinus]
        ;return (opr e1 e2)
       }

-- evaluate fxn, calcualtes values for expressions to test that they are valid 
-- generates [int] representation for a set, need to compare two to evaluate, only does #, not sets of numbers
data Set = Set [Either Int Set]

-- evaluate :: SetExpr -> Set
-- evaluate (Var "A") = Set (map Left [0,1,2,3])
evaluate :: SetExpr -> [Int]
evaluate (Var "A") = [0,1,2,3]
evaluate (Var "B") = [0,2,4,6]
evaluate (Var "C") = [0,1,4,5]
evaluate (Var _v) = [] -- should never reach this case if var assignment happens correctly 
evaluate (Cup e1 e2) = (evaluate e1) `union` (evaluate e2)
evaluate (Vee e1 e2) = (evaluate e1) `union` (evaluate e2)
evaluate (Cap e1 e2) = (evaluate e1) `intersect` (evaluate e2)
evaluate (Wedge e1 e2) = (evaluate e1) `intersect` (evaluate e2)
evaluate (SetMinus e1 e2) = (evaluate e1) \\ (evaluate e2)
evaluate (NotIn e) = [0,1,2,3,4,5,6] \\ (evaluate e)
evaluate (In e) = evaluate e
evaluate (SetBuilder e) = evaluate e
evaluate (Power _e) = [] --powerset (evaluate e)
evaluate (Subset _e) = []--powerset (evaluate e)

-- write a law checker 
-- add distributivity law, a intersectino a = a, a union a = a, (a - b) U b = a U b

-- fxn for building out the powerset of a variable list
powerset :: [a] -> [[a]]
powerset [] = [[]]
powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs


example1, example2, example3, example4, example5, example6, example7 :: SetExpr

example1 = (Cap (Var "M") (Var "N"))
example2 = (Cup (Var "X") (Var "Y"))
example3 = (SetMinus (Var "P") (Var "Q"))
example4 = (Power (Var "K"))
example5 = (In (SetBuilder (Var "p")))
example6 = (Cap (Cap (Var "X") (Var "Y")) (Var "Z"))
example7 = (Cap (Var "X") (Cap (Var "Y") (Var "Z")))

generateProof :: [Law] -> SetExpr -> Proof -- We need only one proof for our problem
generateProof laws' e = Proof e (multiSteps e)
  where multiSteps e' = case [(law_name, res)
                              | (Law law_name law_eq) <- laws'
                              , res <- getStep law_eq e'
                              ] of
                          [] -> []
                          ((nm,e''):_) -> (nm,e'') : multiSteps e''


getStep :: Equation -> SetExpr -> [SetExpr] -- [SetExpr] because we want to check if it's empty or not
getStep eq@(lhs, rhs) e = case match lhs e of
                            [] -> recurse e -- If there is no substition, recursively getStep e
                            (subst:_) -> [apply subst rhs] -- Apply that subst to make rhs
                       where
                            recurse (Var _)          = []
                            recurse (SetBuilder e1)  = [SetBuilder e1' | e1' <- getStep eq e1]
                            recurse (Power e1)       = [Power e1' | e1' <- getStep eq e1]
                            recurse (Cap e1 e2)      = [Cap e1' e2  | e1' <- getStep eq e1] ++
                                                       [Cap e1  e2' | e2' <- getStep eq e2]
                            recurse (Cup e1 e2)      = [Cup e1' e2  | e1' <- getStep eq e1] ++
                                                       [Cup e1  e2' | e2' <- getStep eq e2]
                            recurse (SetMinus e1 e2) = [SetMinus e1' e2  | e1' <- getStep eq e1] ++
                                                       [SetMinus e1  e2' | e2' <- getStep eq e2]
                            recurse (Wedge e1 e2)    = [Wedge e1' e2  | e1' <- getStep eq e1] ++
                                                       [Wedge e1  e2' | e2' <- getStep eq e2]
                            recurse (Vee e1 e2)      = [Vee e1' e2  | e1' <- getStep eq e1] ++
                                                       [Vee e1  e2' | e2' <- getStep eq e2]  
                            recurse (In e1)          = [In e1' | e1' <- getStep eq e1]
                            recurse (NotIn e1)       = [NotIn e1' | e1' <- getStep eq e1]
                            recurse (Subset e1)      = [Subset e1' | e1' <- getStep eq e1]
                            

type Subst = [(String, SetExpr)]

match :: SetExpr -> SetExpr -> [Subst]
match (Var name) e                        = [[(name, e)]]
match (SetBuilder e1) e2                  = match e1 e2
match (Power e1) (Power e2)               = match e1 e2
match (Cap e1 e1') (Cap e2 e2')           = [concat ((match e1 e2) ++ (match e1' e2'))] 
match (Cup e1 e1') (Cup e2 e2')           = [concat ((match e1 e2) ++ (match e1' e2'))]
match (SetMinus e1 e1') (SetMinus e2 e2') = [concat ((match e1 e2) ++ (match e1' e2'))]
match (Wedge e1 e1') (Wedge e2 e2')       = [concat ((match e1 e2) ++ (match e1' e2'))]
match (Vee e1 e1') (Vee e2 e2')           = [concat ((match e1 e2) ++ (match e1' e2'))]
match (In e1) (In e2)                     = match e1 e2
match (NotIn e1) (NotIn e2)               = match e1 e2
match (Subset e1) (Subset e2)             = match e1 e2  
match _ _ = []

apply :: Subst -> SetExpr -> SetExpr
apply subst (Var name)       = lookupInSubst name subst
apply subst (SetBuilder e)   = SetBuilder (apply subst e)
apply subst (Power e)        = Power (apply subst e)
apply subst (Cap e1 e2)      = Cap (apply subst e1) (apply subst e2)
apply subst (Cup e1 e2)      = Cup (apply subst e1) (apply subst e2)
apply subst (SetMinus e1 e2) = SetMinus (apply subst e1) (apply subst e2)
apply subst (Wedge e1 e2)    = Wedge (apply subst e1) (apply subst e2)
apply subst (Vee e1 e2)      = Vee (apply subst e1) (apply subst e2)
apply subst (In e)           = In (apply subst e)
apply subst (NotIn e)        = NotIn (apply subst e)
apply subst (Subset e)       = Subset (apply subst e)


lookupInSubst :: String -> [(String, SetExpr)] -> SetExpr
lookupInSubst nm ((nm',v):rm)
 | nm == nm' = v
 | otherwise = lookupInSubst nm rm
lookupInSubst _ [] = error "Substitution was not complete, or free variables existed in the rhs of some equality"
