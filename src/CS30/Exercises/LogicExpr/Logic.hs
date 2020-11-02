--  then we can get rid of a lot of 'string'
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.LogicExpr.Logic where

import Control.Monad.Combinators.Expr
import CS30.Exercises.LogicExpr.Datatypes
import CS30.Exercises.LogicExpr.Parser
import Text.Megaparsec
import Text.Megaparsec.Char

input_laws = [
    "Law1:¬(¬p)≡p"
    , "Law2:p∧p≡p"
    , "Law3:p∧true≡p"
    , "Law4:p∨true≡true"
    , "Law5:p∨p≡p"
    , "Law6:(p ⇒ q) ≡ ¬p ∨ q"
    , "Law7:p∨false≡p"
    , "Law8:p∧false≡false"
    , "Law9:¬(p∨q)≡¬p∧¬q"
    , "Law10:¬(p∧q)≡¬p∨¬q"
    , "Law11:p∧¬p≡false"
    , "Law12:p∨¬p≡true"
    , "Law3':true∧p≡p"
    , "Law4':true∨p≡true"
    , "Law7':false∨p≡p"
    , "Law8':false∧p≡false"
    ]

-- res = simplify input_laws "¬(p∨q)∨¬(p∨q)∧true∧false"
-- Buggy now

simplify :: [String] -> String -> Calculation
simplify strings str = calculate laws e
  where
    parseLaw s =
        case parse law "" s of
        Left m -> error $ show m
        Right law' -> law'        
    parseExpr s =
        case parse logicExpr "" s of
        Left m -> error $ show m
        Right e' -> e'
    laws = map parseLaw strings
    e = parseExpr str


calculate :: [Law] -> LogicExpr -> Calculation
calculate laws e = Calc e (manyStep rws e)
  where
    rws ex =
      [ (name, e')
        | Law name eqn <- sortLaws laws,
          e' <- rewrites eqn ex,
          e' /= ex
      ]

manyStep :: (LogicExpr -> [Step]) -> LogicExpr -> [Step]
manyStep rws e =
  if null steps
    then []
    else step : manyStep rws (snd step)    
  where
    steps = rws e
    step = head steps

sortLaws = id

rewrites :: Equation -> LogicExpr -> [LogicExpr]

rewrites eqn@(eq1, eq2) e = [apply (unify $ concat $ matchA eq1 e) eqn e]

apply []  _ expr = expr
apply sub (eqn1, eqn2) expr = replace expr eqn1' eqn2'
  where 
    eqn1' = foldl (\acc subst -> applyA acc subst) eqn1 sub
    eqn2' = foldl (\acc subst -> applyA acc subst) eqn2 sub
    replace e e1 e2
      | e == e1 = e2
      | otherwise = case e of 
          (Bin op exp1 exp2) -> (Bin op (replace exp1 e1 e2) (replace exp2 e1 e2))
          (Neg exp) -> (Neg (replace exp e1 e2))
          e -> e    


applyA :: LogicExpr -> (VarName, LogicExpr) -> LogicExpr
applyA e sub = case e of    
    (Var v) -> if v == fst sub then snd sub else (Var v)
    (Neg e1) -> Neg (applyA e1 sub)
    (Bin op e1 e2) -> Bin op (applyA e1 sub) (applyA e2 sub)
    _ -> e

unify xs = if and (map (allSame xs) xs) then unique xs else []
allSame (x:xs) y = if fst x == fst y 
                    then 
                        if snd x == snd y 
                        then allSame xs y
                        else False
                    else
                        allSame xs y
allSame [] _ = True                        

unique xs = reverse $ unique' xs []
unique' (x:xs) acc = if x `elem` acc then unique' xs acc else unique' xs (x:acc)
unique' [] acc = acc

matchA :: LogicExpr -> LogicExpr -> [Substitution]
matchA (Var v) e = [unitSub v e]
matchA eqn@(Bin op e1 e2) (Bin op' e1' e2')
  | op == op' =   if (not $ null m1) && (not $ null m2) 
                  then m1 ++ m2 
                  else if (not $ null m1') && (not $ null m2') 
                       then m1' ++ m2'                      
                       else if null e1_match then e2_match else e1_match
  | otherwise = if null e1_match then e2_match else e1_match
    where
    m1 = (matchA e1 e1')
    m2 = (matchA e2 e2')
    m1' = (matchA e1 e2')
    m2' = (matchA e2 e1')    
    e1_match = matchA eqn e1'
    e2_match = matchA eqn e2'
matchA (Neg e1) (Neg e2) = matchA e1 e2
matchA (Con True) (Con True) = [[]]
matchA (Con True) _ = []
matchA (Con False) (Con False) = [[]]
matchA (Con False) _ = []
matchA _ _ = []



-- neglaw = parse law "" "neg law:¬(¬p) ≡ p"

-- law2 =  parseLaw "onelaw: p∨p≡p"
-- exp2 = parseExpr "(p∨p)"
-- Law name eqn2 = law2

-- law3 = parseLaw "Law3: p⇒q≡¬p∨q"
-- exp3 = parseExpr "p⇒r"
-- Law name3 eqn3 = law3
-- (eqn31, eqn32) = eqn3

-- law4 = parseLaw "Law3:p∧false≡false"
-- -- exp4 = parseExpr "false∧q∧true∧p"
-- exp4 = parseExpr "¬(p ⇒ q) ⇒ (p ∧ ¬q)"

-- -- exp4 = parseExpr "false∧q"
-- Law name4 eqn4 = law4
-- (eqn41, eqn42) = eqn4
