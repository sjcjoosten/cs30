module CS30.Exercises.LogicRewriting where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Control.Monad.Combinators.Expr
import           Data.Void
import           Data.Functor.Identity

data Law = Law LawName Equation
    deriving Show
type LawName = String
type Equation = (Expr,Expr)

data Expr = Var Char        -- variable like 'p' or 'q' 
          | Const Bool        -- either true or false
          | And Expr Expr     -- <expr> \wedge <expr> 
          | Or Expr Expr      -- <expr> \vee <expr>
          | Implies Expr Expr -- <expr> \Rightarrow <expr>
          | Neg Expr          -- negation (\neg <expr>)
    deriving Show

type Parser = ParsecT Void String Identity

-- parses laws in a format like:
--   "double negation: \\neg (\\neg p) = p"
parseLaw :: Parser Law
parseLaw = do name  <- someTill anySingle (string ":") <* spaceConsumer
              expr1 <- parseExpr
              _     <- symbol "=" 
              expr2 <- parseExpr
              return (Law name (expr1,expr2))

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

-- parse some lexeme, accounting for spaces
lexeme :: Parser a -> Parser a
lexeme   = L.lexeme spaceConsumer

-- parse something between (...) 
parens :: Parser a -> Parser a          
parens = between (symbol "(") (symbol ")")

-- operator table for makeExprParser
operatorTable :: [[Operator Parser Expr]]
operatorTable = 
    [ [prefix "\\neg" Neg],
      [binary "\\vee" Or, 
       binary "\\wedge" And, 
       binary "\\Rightarrow" Implies]
    ]

-- helper function for generating an binary infix operator
-- based on documentation for Control.Monad.Combinators.Expr
binary :: String -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ symbol name)

-- helper function for prefix operators 
-- also from Control.Monad.Combinators.Expr documentation 
prefix :: String -> (a -> a) -> Operator Parser a
prefix name f = Prefix  (f <$ symbol name)

-- parse variable (any single lowercase character)
parseVar :: Parser Expr
parseVar = Var <$> lexeme lowerChar

-- parse constants (either literal "true" or "false")
parseConst :: Parser Expr
parseConst = parseTrue <|> parseFalse
    where parseTrue = do {_ <- symbol "true"; return (Const True)}
          parseFalse = do {_ <- symbol "false"; return (Const False)}

-- parse a term (either something in parens, a constant "true"/"false",
-- or a variable)
parseTerm :: Parser Expr
parseTerm = parens parseExpr <|> parseConst <|> parseVar

-- parse a full expression (using makeExprParser)
parseExpr :: Parser Expr
parseExpr =  makeExprParser parseTerm operatorTable
