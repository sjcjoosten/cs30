{-# OPTIONS_GHC -Wall #-}
module CS30.Exercises.ModuloProof where

import CS30.Exercises.ModularArithmetic.ModuloParser

import Control.Monad.IO.Class (MonadIO(liftIO))
import System.Random (randomRIO)
import System.Random
import Control.Monad
import Control.Monad.ST
import Data.STRef

------------------------------------------Implicit Laws----------------------------------------

-- modulo_laws :: [Law]
-- modulo_laws = map (parseLaw False Congruent)
--               [
--                "Law2 : p \\equiv_p 0 (mod p)",
--                "Law1 : x ^ (p-1) \\equiv_p 1 (mod p)"
--               ]

-- TODO : add artihmetic laws, 0 ,1 , mul, add, power, handle 0 ^ 0
arithmetic_laws :: [Law]
arithmetic_laws = map (parseLaw False)
                  [
                   "Law1 : x * 0 = 0"
                   ,"Law2 : x + 0 = x"
                   ,"Law3 : x * 1 = x"
                   ,"Law4 : x * -1 = -x"
                   ,"Law5 : x ^ 0 = 1"
                   ,"Law6 : 1 ^ x = 1"
                  ,  "Law7 : x + y = y + x"
                  ,  "Law8 : x * y = y * x"
                  ,  "Law9 : (x + y) + z = x + (y + z)"
                  ,  "Law10 : (x * y) * z = x * (y * z)"
                   ,"Law11 : x * (y + z) = x * y + x * z"
                   ,"Law12 : x * (y - z) = x * y - x * z"
                   ,"Law14 : (x + y) * z = x * z + y * z"
                   ,"Law15 : (x - y) * z = x * z - y * z"
                   ,"Law13 : x ^ (y + z) = x ^ y * x ^ z"
                  --  ,"Law99 : a \\equiv_p b = a + c \\equiv_p b + c"
                  ]
                  
-------------------------------------Proofs----------------------------------------

getDerivation :: Int -> [Law] -> Expression -> Proof
getDerivation steps laws e
 = Proof e (multiSteps steps e)
 where multiSteps 0 _ = []
       multiSteps steps' e'
        = case [ (lawName law, res)
               | law <- laws
               , res <- getStep (lawEq law) e'
               ] of
                   [] -> []
                   ((nm,e''):_) -> (nm,e'') : multiSteps (steps'-1) e''

-- would go into rewrite function
-- Law1 : a = b (mod p) implies a + c = b + c (mod p)
-- Law2 : a = b (mod p) implies c + a = c + b (mod p)
-- Law3 : a = b (mod p) implies a * c = b * c (mod p)
-- Law4 : a = b (mod p) implies c * a = c * b (mod p)
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
matchE (Fixed i) (Fixed j) | i == j = Just [] -- TODO : Confirm
matchE (Fixed _) _ = Nothing -- TODO : Confirm
matchE (BinOp op1 e1 e2) (BinOp op2 e3 e4) | op1 == op2
 = case (matchE e1 e3, matchE e2 e4) of
    (Just s1, Just s2) -> combineTwoSubsts s1 s2
    _ -> Nothing
matchE (BinOp _ _ _) _ = Nothing
matchE (UnOp Neg e1) (UnOp Neg e2) = matchE e1 e2
matchE (UnOp _ _) _ = Nothing
matchE ExpressionError _ = Nothing

combineTwoSubsts :: Substitution -> Substitution -> Maybe Substitution
combineTwoSubsts s1 s2
  = case and [v1 == v2 | (nm1,v1) <- s1, (nm2,v2) <- s2, nm1 == nm2] of
     True -> Just (s1 ++ s2)
     False -> Nothing

apply :: Substitution -> Expression -> Expression
apply subst (Var nm) = lookupInSubst [nm] subst
apply _ (Con i) = Con i
apply _ (Fixed i) = Fixed i -- TODO : definitely not, change
apply subst (UnOp op e) = UnOp op (apply subst e)
apply subst (BinOp op e1 e2) = BinOp op (apply subst e1) (apply subst e2)
apply _ ExpressionError = ExpressionError

lookupInSubst :: String -> [(String, p)] -> p
lookupInSubst nm ((nm',v):rm)
 | nm == nm' = v
 | otherwise = lookupInSubst nm rm
lookupInSubst _ [] = error "Substitution was not complete, or free variables existed in the rhs of some equality"

isPrime :: Int -> Bool -- TODO : Replace
isPrime k = elem k [2,3,5,7,11,13,17,19,23,29,31]

-------------------------------------Tests----------------------------------------

exp2 :: Expression
exp2 = parseExpression True "(a + b) * (a + c)"

given1 :: Law
given1 = parseLaw True "given1 : a = b + c"

prf1 :: Proof
prf1 = getDerivation 10 (given1:arithmetic_laws) exp2

-- -- lawsStr :: String
-- -- lawsStr = intercalate "\n" (map showLaw arithmetic_laws) ++ "\n" ++ 
-- --           intercalate "\n" (map showLaw modulo_laws) ++ "\n"
