module CS30.Exercises.LogicExpr.Parser where

import CS30.Exercises.LogicExpr.Datatypes

import           Text.Megaparsec
import           Text.Megaparsec.Char
import Control.Monad.Combinators.Expr

negStr = "¬"
-- negStr = "-"
andStr = "∧"
-- andStr = "^"
orStr = "∨"
-- orStr = "v"
implyStr = "⇒"
-- implyStr = "=>"
equalStr= "≡"

logicExpr :: Parser LogicExpr
logicExpr = space *> makeExprParser term ops <* space
    where
        term = (do 
            v <- satisfy (\v -> v=='p' || v=='q' || v=='r') <* space
            return (Var v)
            )
            <|>
            (do
                v <- (string "true" <|> string "false") <* space
                return (Con (if v == "true" then True else False)))
            <|>
            (do 
                string "(" *> space
                t <- logicExpr
                string ")" *> space
                return t
                )

        ops = [
            [ Prefix (return Neg <* string negStr <* space)]
            , [ infixL andStr And
                , infixL orStr Or]
            , [infixL implyStr Imply]
            ]            
        space = ((string " " <|> string "\\t" <|> string "\\ ") *> space)
                <|> return ()            
        infixL str op = InfixL (return (Bin op) <* string str <* space)


law :: Parser Law
law = do
    name <- parseUntil ':' <* space    
    e1 <- logicExpr <* space
    string equalStr
    e2 <- logicExpr <* space
    return (Law name (e1,e2))


-- runParser logicExpr "" "pVq^true"
-- runParser law "" "fei  : -(-p) ≡ p"
parseUntil c = 
    do 
        satisfy (== c)
        return []
    <|> 
    do
        x <- satisfy (const True)
        rmd <- parseUntil c
        return (x:rmd)


