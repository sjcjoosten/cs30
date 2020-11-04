module CS30.Exercises.SetConversionProofs.GenerateProof  where
import CS30.Data
import CS30.Exercises.Data
import CS30.Exercises.SetConversionProofs.SetExprParser
import CS30.Exercises.SetConversionProofs.LawParser


data Proof = Proof SetExpr [(String, SetExpr)] deriving Show


generateRandomSetExpr :: Int -> ChoiceTree SetExpr
generateRandomSetExpr = undefined


getProof :: [Law] -> SetExpr -> Proof -- We need only one proof for our problem
getProof laws' e = Proof e (multiSteps e)
  where multiSteps e' = case [(law_name, res)
                              | (Law law_name law_eq) <- laws' -- get all laws
                              , res <- getStep law_eq e' -- try to use that law equation
                              ] of
                          [] -> []
                          ((nm,e''):_) -> (nm,e'') : multiSteps e'' -- if we found a step, continue


getStep :: Equation -> SetExpr -> [SetExpr] -- [SetExpr] because we want to cehck if it is empty or not
getStep (lhs, rhs) e = case match lhs e of    -- "match" creats a substition list
                            [] -> recurse e -- If there is no substition, expand e?
                            (subst:_) -> [apply subst rhs] -- Apply that subst to make rhs
                       where recurse = undefined

type Subst = [(String, SetExpr)]

-- Law 1: Law "cap definition" (Cap (Var "A") (Var "B"), SetBuilder (Wedge (In (Var "A")) (In (Var "B"))))

match :: SetExpr -> SetExpr -> [Subst] -- Creates a substition list
match (Var name) exp = [(name, exp)]
match (Cap _) (Cap _) = undefined


apply :: Subst -> SetExpr -> SetExpr
apply subst (Var name) = lookupInSubst name subst
apply = undefined


lookupInSubst :: String -> [(String, p)] -> p
lookupInSubst = undefined
-- lookupInSubst nm ((nm',v):rm)
--  | nm == nm' = v
--  | otherwise = lookupInSubst nm rm
-- lookupInSubst _ [] = error "Substitution was not complete, or free variables existed in the rhs of some equality"

















---------------------------------------------------------------------------------------

-- data Calculation =  Calc SetExpr [Step]
-- data Step = Step LawName SetExpr

-- calculate :: [Law] -> SetExpr -> Calculation
-- calculate laws e = Calc e (manySteps rws e)
--   where rws e = [Step name e' | Law name eq <- laws, e' <- rewrites eq e]


-- manySteps :: (SetExpr -> [Step]) -> SetExpr -> [Step]
-- manySteps rws e
--   = case steps of 
--     [] -> []
--     (o@(Step _ e):_) -> o:manySteps rws e
--   where steps = rws e


-- rewrites :: Equation -> SetExpr -> [SetExpr]
-- rewrites eqn (Compose as) = map Compose (
--                 rewritesSeg eqn as ++ anyOne (rewritesA eqn) as)

-- rewritesSeg :: Equation -> [SetExpr] -> [[SetExpr]]
-- rewritesSeg (e1, e2) as
--   = [as1 ++ deCompose (apply subst e2) ++ as3
--     | (as1, as2, as3) <- (splitsN 3 as)
--     , subst <- match (e1, Compose as2) ]

-- rewritesA eqn (Var v) = []
-- rewritesA eqn (Con k es) = map (Con k) (anyOne (rewrites eqn) es)


-- anyOne :: (a -> [a]) -> [a] -> [[a]]
-- anyOne f []     = []
-- anyOne f (x:xs) = [x':xs | x' <- f x] ++ 
--                   [x:xs' | xs' <- anyOne f xs]


-- splits :: [a] -> [([a], [a])]
-- splits [] = [([], [])]
-- splits (a:as) = [([], a:as)] ++ [(a:as1, as2) | (as1, as2) <- splits as]
-- -- splits as = [(break n as) | n <- [0 .. length as]]


-- -- splitsN :: Int -> [a] -> [[[a]]]
-- -- splitsN 0 [] = [[]]
-- -- splitsN 0 as = []
-- -- splitsN n as = [bs : bss 
-- --                | (bs,cs) <- splits as
-- --                , bss <- splitsN (n-1) cs]


-- -- alignments :: (SetExpr,SetExpr) -> [([Atom], SetExpr)]
-- -- alignments (Compose as, Compose bs)
-- --   = [zip as (map Compose bss) | bss <- splitsN (length as) bs]

-- match :: (SetExpr,SetExpr) -> [Subst]
-- match = concatMap (combine . map matchA) . alignments

-- matchA :: (Atom, SetExpr) -> [Subst]
-- matchA (Var v, e) = [unitSub v e]
-- matchA (Con k1 es1, Compose [Con k2 es2]) | k1 == k2
--   = combine (map match (zip es1 es2))

-- type VarName = String
-- type Subst = [(VarName, SetExpr)]

-- unitSub :: VarName -> SetExpr -> Subst
-- unitSub v e = [(v,e)]

-- apply :: Subst -> SetExpr -> SetExpr
-- apply sub (Compose as) = Compose (concatMap (applyA sub) as)

-- applyA :: Subst -> Atom -> [Atom]
-- applyA sub (Var v) = deCompose (binding sub v)
-- applyA sub (Con k es) = [Con k (map (apply sub) es)]

-- binding :: Subst -> VarName -> SetExpr
-- binding ((v',e):sub) v 
--                     | v' == v = e
--                     | otherwise = binding sub v
-- binding [] v = error "Could not find binding"

-- combine :: [[Subst]] -> [Subst]
-- combine = filterUnifiable . cp

-- filterUnifiable = concatMap unifyAll

-- unifyAll :: [Subst] -> [Subst]
-- unifyAll = foldr f e
--   where f sub subs  = concatMap (unify sub) subs
--         e = [[]]

-- unify :: Subst -> Subst -> [Subst]
-- unify s1 s2 = if compatible s1 s2 then [s1 ++ s2] else []

-- compatible :: Subst -> Subst -> Bool
-- compatible sub1 sub2 = and [e1==e2 | (v1, e1) <- sub1
--                                     ,(v2, e2) <- sub2
--                                     , v1 == v2]

-- cp :: [[a]] -> [[a]]
-- cp []       = [[]]
-- cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]


-- -- deCompose :: SetExpr -> [Atom]
-- -- deCompose (Compose as) = as
