{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
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
prec And = 2
prec Or =  2
prec Imply = 1  

showSpace :: ShowS
showSpace = showChar ' '
showOp :: LogicOp -> ShowS
showOp = showString . symb

symb :: LogicOp -> String
symb And = andStr
symb Or = orStr
symb Imply = implyStr

instance Show LogicExpr where
    showsPrec _ (Con b) = showString (show b)
    showsPrec _ (Var v) = showString [v]    
    showsPrec p (Neg e) = showParen (p > 3) (showString negStr . showsPrec (4) e)
    showsPrec p (Bin op e1 e2)
        = showParen (p >= q) (showsPrec q e1 . showSpace . showOp op . showSpace . showsPrec (q+1) e2) 
        where q = prec op    

instance Show LogicOp where
    show And = andStr
    show Or = orStr
    show Imply = implyStr

data Law = Law {lawName :: LawName, lawEq :: Equation} deriving (Show)
type LawName = String
type Equation = (LogicExpr,LogicExpr)

parseLaw :: String -> Law
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
        term = (do 
            v <- satisfy (\v -> v=='p' || v=='q' || v=='r') <* space'
            return (Var v)
            )
            <|>
            (do
                v <- (string "true" <|> string "false") <* space'
                return (Con (if v == "true" then True else False)))
            <|>
            (do 
                string "(" *> space'
                t <- logicExpr
                string ")" *> space'
                return t
                )

        ops = [
            [ Prefix (return Neg <* string negStr <* space')]
            , [ infixL andStr And
                , infixL orStr Or]
            , [infixL implyStr Imply]
            ]            
        space' = ((string " " <|> string "\\t" <|> string "\\ ") *> space')
                <|> return ()            
        infixL str op = InfixL (return (Bin op) <* string str <* space')


law :: Parser Law
law = do
    name <- parseUntil ':' <* space    
    e1 <- logicExpr <* space
    _ <- string equalStr
    e2 <- logicExpr <* space
    return (Law name (e1,e2))


parseUntil :: MonadParsec e s f => Token s -> f [Token s]
parseUntil c = 
    do 
        _ <- satisfy (== c)
        return []
    <|> 
    do
        x <- satisfy (const True)
        rmd <- parseUntil c
        return (x:rmd)


