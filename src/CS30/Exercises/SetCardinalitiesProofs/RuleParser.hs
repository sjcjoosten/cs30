module CS30.Exercises.SetCardinalitiesProofs.RuleParser where
import           Control.Monad.Combinators.Expr
import           Data.Functor.Identity
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

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
         deriving (Show, Eq)

data Expr = Var Char | Op Symb [Expr] deriving (Show, Eq)

data Law = Law {lawName :: String, lawEquation :: Equation} deriving (Show)
type Equation = (Expr, Expr)  -- (left,right)



pLaw :: Parser Expr -> Parser Law
pLaw pExpr = 
   do lawNm <- parseUntil ':' <*spaces
      lhs <- pExpr
      _ <- string "=" <*spaces
      rhs <- pExpr
      return (Law lawNm (lhs, rhs))

parseUntil :: MonadParsec e s f => Token s -> f [Token s]
parseUntil c 
   =  (do _ <- satisfy (== c) 
          return []) <|>
      (do ch <- satisfy (const True) 
          rmd <- parseUntil c 
          return (ch:rmd))

-- operator table for use with makeExprParser 
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ 
     -- Number operations
     [binary "^" Expon],
     [binary "\\cdot" Mult],
     [binary "+" Add, binary "-" Sub],

   --   -- Set operations
     [binary "\\times" Cartesian],
     [binary "\\cup" Union],
     [binary "\\cap" Intersection],
     [binary "\\setminus" Setminus]
  ]

binary :: String -> Symb -> Operator (ParsecT Void String Data.Functor.Identity.Identity) Expr
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
spaces = many spc >> return ()
 where spc = string " " <|> string "\t" <|> string "\n" <|> string "\r"
             <|> string "\\ " <|> string "~"

-- parse a given specific string, accounting for spaces 
symbol :: String -> Parser String
symbol = unspace . string

unspace :: Parser a -> Parser a
unspace p = spaces *> p <* spaces

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

parseVar :: Parser Expr
parseVar = Var <$> satisfy isLetter <* spaces
   where
      isLetter c = elem c ['A'..'Z']

parseTerm :: Parser Expr
parseTerm =  brackets parseExpr <|> parseCardinality <|> parseVar -- <|> parseBinom <|> parseFrac <|> parens parseExpr <|>


parseExpr :: Parser Expr
parseExpr =  makeExprParser parseTerm operatorTable



-- Given Rules:
-- |A \cup B| = |A| + |B| - |A \cap B|
-- |A \times B| = |A| \cdot |B|
-- |\P(A)| = 2^{|A|}
-- |A \setminus B| = |A| - |A \cap B|
