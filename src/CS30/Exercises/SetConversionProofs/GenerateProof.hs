module CS30.Exercises.SetConversionProofs.GenerateProof  where
import CS30.Data
import CS30.Exercises.Data
import CS30.Exercises.SetConversionProofs.SetExprParser
import CS30.Exercises.SetConversionProofs.LawParser


type LawName = String

-- difining types here for testing purpose
-- newtype SetExpr = Compose [Atom] deriving Eq
-- data Atom    = Var String -- single variable
--               | SetBuilder SetExpr -- set builder expression
--               | Power SetExpr   -- powerset
--               | Cap SetExpr SetExpr     -- cap operation, intersection
--               | Cup SetExpr SetExpr     -- cup operation, union
--               | SetMinus  SetExpr SetExpr  -- set difference
--               | Wedge SetExpr SetExpr   -- and, intersection
--               | Vee SetExpr SetExpr     -- or, union
--               | In SetExpr      -- element of 
--               | NotIn SetExpr   -- not an element of
--               | Subset SetExpr -- subset of
--               deriving (Show, Eq)



data Calculation =  Calc SetExpr [Step]
data Step = Step LawName SetExpr

calculate :: [Law] -> SetExpr -> Calculation
calculate laws e = Calc e (manySteps rws e)
  where rws e = [Step name e' | Law name eq <- laws, e' <- rewrites eq e]


manySteps :: (SetExpr -> [Step]) -> SetExpr -> [Step]
manySteps rws e
  = case steps of 
    [] -> []
    (o@(Step _ e):_) -> o:manySteps rws e
  where steps = rws e


rewrites :: Equation -> SetExpr -> [SetExpr]
rewrites eqn (Compose as) = map Compose (
                rewritesSeg eqn as ++ anyOne (rewritesA eqn) as)

rewritesSeg :: Equation -> [SetExpr] -> [[SetExpr]]
rewritesSeg (e1, e2) as
  = [as1 ++ deCompose (apply subst e2) ++ as3
    | (as1, as2, as3) <- (splitsN 3 as)
    , subst <- match (e1, Compose as2) ]

rewritesA eqn (Var v) = []
rewritesA eqn (Con k es) = map (Con k) (anyOne (rewrites eqn) es)


anyOne :: (a -> [a]) -> [a] -> [[a]]
anyOne f []     = []
anyOne f (x:xs) = [x':xs | x' <- f x] ++ 
                  [x:xs' | xs' <- anyOne f xs]


splits :: [a] -> [([a], [a])]
splits [] = [([], [])]
splits (a:as) = [([], a:as)] ++ [(a:as1, as2) | (as1, as2) <- splits as]
-- splits as = [(break n as) | n <- [0 .. length as]]


splitsN :: Int -> [a] -> [[[a]]]
splitsN 0 [] = [[]]
splitsN 0 as = []
splitsN n as = [bs : bss 
               | (bs,cs) <- splits as
               , bss <- splitsN (n-1) cs]


alignments :: (SetExpr,SetExpr) -> [([Atom], SetExpr)]
alignments (Compose as, Compose bs)
  = [zip as (map Compose bss) | bss <- splitsN (length as) bs]

match :: (SetExpr,SetExpr) -> [Subst]
match = concatMap (combine . map matchA) . alignments

matchA :: (Atom, SetExpr) -> [Subst]
matchA (Var v, e) = [unitSub v e]
matchA (Con k1 es1, Compose [Con k2 es2]) | k1 == k2
  = combine (map match (zip es1 es2))

type VarName = String
type Subst = [(VarName, SetExpr)]

unitSub :: VarName -> SetExpr -> Subst
unitSub v e = [(v,e)]

apply :: Subst -> SetExpr -> SetExpr
apply sub (Compose as) = Compose (concatMap (applyA sub) as)

applyA :: Subst -> Atom -> [Atom]
applyA sub (Var v) = deCompose (binding sub v)
applyA sub (Con k es) = [Con k (map (apply sub) es)]

binding :: Subst -> VarName -> SetExpr
binding ((v',e):sub) v 
                    | v' == v = e
                    | otherwise = binding sub v
binding [] v = error "Could not find binding"

combine :: [[Subst]] -> [Subst]
combine = filterUnifiable . cp

filterUnifiable = concatMap unifyAll

unifyAll :: [Subst] -> [Subst]
unifyAll = foldr f e
  where f sub subs  = concatMap (unify sub) subs
        e = [[]]

unify :: Subst -> Subst -> [Subst]
unify s1 s2 = if compatible s1 s2 then [s1 ++ s2] else []

compatible :: Subst -> Subst -> Bool
compatible sub1 sub2 = and [e1==e2 | (v1, e1) <- sub1
                                    ,(v2, e2) <- sub2
                                    , v1 == v2]

cp :: [[a]] -> [[a]]
cp []       = [[]]
cp (xs:xss) = [x:ys | x <- xs, ys <- cp xss]


deCompose :: SetExpr -> [Atom]
deCompose (Compose as) = as

----------------------------------------------
-- from DEMO
----------------------------------------------
data Proof = Proof SetExpr [(String, SetExpr)]

getProofLengthN :: Int -> [Law] -> (SetExpr -> Equation -> [SetExpr])
                -> SetExpr -> [Proof]
-- helped by Le
-- getProofLengthN n ((Law lwnm eqn):xs) fnc e = case fnc e eqn of 
--   Nothing -> getProofLengthN n xs fnc e
--   Just sth -> let Proof _ steps = getProofLengthN (n-1) ((Law lwnm eqn):xs) fnc sth in Proof e ((lwnm, sth):steps
getProofLengthN 0 _ _ e = [Proof e []]
getProofLengthN n lows fnc e = 
  [Proof e ((lwnm, (fnc e eqn)):steps) | (Law lwnm eqn) <- lws, (Proof _ steps) <- getProofLengthN (n-1) lws fnc (fnc e eqn)]

fnc:: SetExpr -> Equation -> Maybe SetExpr
fnc e (lh,rh) = if e == lh then Just (rewrites e rh) else Nothing
