import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser = Parsec Void String
data Symb = Add | Minus | Intersection | Union | Powerset | Card | Const Char
data Expr = Var Symb | Op Symb [Expr]

data Law = Law String Equation
type Equation = (Expr, Expr)  -- (left,right)



pLaw :: Parser Expr -> Parser Law
pLaw pExpr = do 
    lawName  <- parseUntil ':'
    _ <- ":"
    lhs <- pExpr
    _ <- "="
    rhs <- pExpr
    return (Law lawName (lhs, rhs))

parseUntil c = (do _ <- satisfy (== c)
                return []) <|>
             (do c <- satisfy (const True)
                rmd <- parseUntil c
                return (c:rmd)
             )





-- Given Rules:
-- |A \cup B| = |A| + |B| - |A \cap B|
-- |A \times B| = |A| \cdot |B|
-- |\P(A)| = 2^{|A|}
-- |A \setminus B| = |A| - |A \cap B|
