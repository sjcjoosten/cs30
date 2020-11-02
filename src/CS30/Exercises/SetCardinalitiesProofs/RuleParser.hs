import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad.Combinators.Expr

import qualified Text.Megaparsec.Char.Lexer as L


type Parser = Parsec Void String
data Symb = Add 
         | Sub 
         | Mult
         | Intersection 
         | Union 
         | Powerset 
         | Cardinality
         | Cartesian
         | Expon
         | Setminus

type Const = Char
data Expr = Var Const | Op Symb [Expr]

data Law = Law String Equation
type Equation = (Expr, Expr)  -- (left,right)



pLaw :: Parser Expr -> Parser Law
pLaw pExpr = 
   do lawName <- parseUntil ':'
      lhs <- pExpr
      _ <- string "="
      rhs <- pExpr
      return (Law lawName (lhs, rhs))

parseUntil :: MonadParsec e s f => Token s -> f [Token s]
parseUntil c 
   =  (do _ <- satisfy (== c) 
          return []) <|>
      (do c <- satisfy (const True) 
          rmd <- parseUntil c 
          return (c:rmd))

-- operator table for use with makeExprParser 
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ 
     -- Number operations
     [binary "\\cdot" Mult],
     [binary "+" Add],
     [binary "-" Sub],
     [binary "^" Expon],

   --   -- Set operations
     [binary "\\cup" Union],
     [binary "\\cap" Intersection],
     [binary "\\times" Cartesian ],
     [binary "\\setminuts" Setminus]
  ]

binary name symb = InfixL ((\lhs rhs -> Op symb [lhs,rhs]) <$ symbol name)


-- parses some integer number alone into MathExpr
-- parseConstant :: Parser Expr
-- parseConstant = do
--   n <- lexeme L.decimal
--   return (Const n)

--next few functions taken from Donia and Bennetts Exercise:

-- parse spaces (used w/ symbol and lexeme)
spaceConsumer :: Parser ()
spaceConsumer = L.space spaces empty empty 

-- based on Drill 6.2 scaffold
spaces :: Parser ()
spaces = some spc >> return ()
 where spc = string " " <|> string "\t" <|> string "\n" <|> string "\r"
             <|> string "\\ " <|> string "~"

-- parse a given specific string, accounting for spaces 
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

-- parse something between {...}
brackets :: Parser a -> Parser a
brackets = between (symbol "{") (symbol "}")

-- parse something between |...|
pipes :: Parser a -> Parser a
pipes = between (symbol "|") (symbol "|")

-- parse something between {...}
parseCardinality :: Parser Expr
parseCardinality = do
   x <- pipes parseExpr
   return (Op Cardinality [x])
-- cardinality = between (symbol "|") (symbol "|")


parseTerm :: Parser Expr
parseTerm =  brackets parseExpr <|> parseCardinality-- <|> parseConstant -- <|> parseBinom <|> parseFrac <|> parens parseExpr <|>


parseExpr :: Parser Expr
parseExpr =  makeExprParser parseTerm operatorTable



-- Given Rules:
-- |A \cup B| = |A| + |B| - |A \cap B|
-- |A \times B| = |A| \cdot |B|
-- |\P(A)| = 2^{|A|}
-- |A \setminus B| = |A| - |A \cap B|
