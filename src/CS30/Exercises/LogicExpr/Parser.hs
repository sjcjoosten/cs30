{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}
module CS30.Exercises.LogicExpr.Parser where
import           Text.Megaparsec
import           Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import           Data.Void
import           Data.Aeson as JSON -- used for 'deriveJSON' line
import           Data.Aeson.TH

type Parser = Parsec Void String
data LogicExpr 
    = Con Bool
    | Var Char
    | Bin LogicOp LogicExpr LogicExpr
    | Neg LogicExpr
    deriving (Eq)
    -- deriving (Eq, Show)

data LogicOp
    = And
    | Or
    | Imply
    deriving (Eq)
    -- deriving (Eq, Show)
$(deriveJSON defaultOptions ''LogicOp)
$(deriveJSON defaultOptions ''LogicExpr)

equivalenceCheck :: LogicExpr -> LogicExpr -> Bool
equivalenceCheck lhs rhs = and (
    do 
        p <- [('p', True), ('p', False)]
        q <- [('q', True), ('q', False)]
        r <- [('r', True), ('r', False)]
        let assoc = [p, q, r]
            lv = evaluate' lhs assoc
            rv = evaluate' rhs assoc        
        return (lv == rv)
    )    


evaluate' :: LogicExpr -> [(Char, Bool)] -> Bool
evaluate' (Con b) _assoc = b
evaluate' (Var v) assoc = case lookup v assoc of  Just v' -> v'; Nothing -> error "Var not in assoc table"
evaluate' (Neg e) assoc = not $ evaluate' e assoc
evaluate' (Bin And e1 e2) assoc = evaluate' e1 assoc && evaluate' e2 assoc
evaluate' (Bin Or e1 e2) assoc = evaluate' e1 assoc || evaluate' e2 assoc
evaluate' (Bin Imply e1 e2) assoc = not (evaluate' e1 assoc) || evaluate' e2 assoc


prec :: LogicOp -> Int
prec And = 3
prec Or =  2
prec Imply = 1  

negPrec :: Int
negPrec = 4

showSpace :: ShowS
showSpace = showChar ' '
showOp :: LogicOp -> ShowS
showOp = showString . symb

symb :: LogicOp -> String
symb And = andStr
symb Or = orStr
symb Imply = implyStr

instance Show LogicExpr where
    show e = shows e ""
    showsPrec _ (Con b) = showString (if b then "true" else "false")
    showsPrec _ (Var v) = showString [v]       
    showsPrec _p (Neg e) = showString negStr . showsPrec negPrec e
    showsPrec p (Bin Imply e1 e2)
        = showParen (p /= 0) (showsPrec q e1 . showSpace . showOp Imply . showSpace . showsPrec q e2) 
        where q = prec Imply
    showsPrec p (Bin op e1 e2)
        = showParen (p /= 0 && p /= q) (showsPrec q e1 . showSpace . showOp op . showSpace . showsPrec q e2) 
        where q = prec op

instance Show LogicOp where
    show And = andStr
    show Or = orStr
    show Imply = implyStr

data Law a = Law {lawName :: a, lawEq :: Equation} deriving (Show)
type LawName = String
type Equation = (LogicExpr,LogicExpr)



parseLaw :: String -> Law LawName
parseLaw s =
    case parse law "" s of
    Left m -> error $ show m
    Right law' -> law'   

parseExpr :: String -> LogicExpr
parseExpr s =
    case parse logicExpr "" s of
    Left m -> error $ show m
    Right e' -> e'

negStr :: String; andStr :: String; orStr :: String; implyStr :: String; equalStr :: String
(negStr, andStr, orStr, implyStr, equalStr) = ("¬", "∧", "∨", "⇒", "≡")

logicExpr :: Parser LogicExpr
logicExpr = space' *> makeExprParser term ops <* space'
    where
        term = 
            (do 
                v <- satisfy (\v -> v=='p' || v=='q' || v=='r') <* space'
                return (Var v))
            <|>
            (do
                v <- (string "true" <|> string "false") <* space'
                return (Con (v == "true")))
            <|>
            (do 
                string "(" *> space'
                t <- logicExpr
                string ")" *> space'
                return t)
        ops = [
            [ Prefix (Neg <$ string negStr <* space')]
            , [ infixL andStr And
                , infixL orStr Or]
            , [infixL implyStr Imply]
            ]            
        infixL str op = InfixL (Bin op <$ string str <* space')

space' :: Parser ()
space' = ((string " " <|> string "\\t" <|> string "\\ ") *> space') <|> return ()            


law :: Parser (Law LawName)
law = do
    name <- parseUntil ':' <* space'    
    e1 <- logicExpr <* space'
    _ <- string equalStr
    e2 <- logicExpr <* space'
    return (Law name (e1,e2))

parseUntil :: Char -> Parser String
parseUntil c = 
    do 
        _ <- satisfy (== c)
        return []
    <|> 
    do
        x <- satisfy (const True)
        rmd <- parseUntil c
        return (x:rmd)


