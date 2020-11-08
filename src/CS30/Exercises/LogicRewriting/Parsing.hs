module CS30.Exercises.LogicRewriting.Parsing where

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

data Expr = Var Char          -- variable like 'p' or 'q' 
          | Const Bool        -- either true or false
          | And Expr Expr     -- <expr> \wedge <expr> 
          | Or Expr Expr      -- <expr> \vee <expr>
          | Implies Expr Expr -- <expr> \Rightarrow <expr>
          | Neg Expr          -- negation (\neg <expr>)
    deriving (Eq, Show)

type Parser = ParsecT Void String Identity

-- contains all the laws that we use (after parsing)
lawStrings :: [String]
lawStrings = [
        "Double Negation Law: \\neg (\\neg p) \\equiv p",
        "Identity Law: p \\vee false \\equiv p",
        "Identity Law: p \\wedge true \\equiv p",
        "Domination Law: p \\wedge false \\equiv false",
        "Domination Law: p \\vee true \\equiv true",
        "Idempotent Law: p \\vee p \\equiv p",
        "Idempotent Law: p \\wedge p \\equiv p",
        "Implication Law: p \\Rightarrow q \\equiv \\neg p \\vee q",
        "De Morgan's Law: \\neg (p \\wedge q) \\equiv \\neg p \\vee \\neg q",
        "De Morgan's Law: \\neg (p \\vee q) \\equiv \\neg p \\wedge \\neg q",
        "Negation Law: p \\vee \\neg p \\equiv true",
        "Negation Law: p \\wedge \\neg p \\equiv false"
    ]

-- parse all the laws into our Law data structure
laws :: [Law]
laws = map prsLaw lawStrings
       where prsLaw x = case parse parseLaw "" x of
                           Left _  -> error ""
                           Right l -> l

-- just the (unique) names of the laws
lawNames :: [String]
lawNames = uniques [name | (Law name _) <- laws]
           where uniques [] = []
                 uniques (x:xs) = if x `elem` xs then uniques xs
                                  else x:(uniques xs)

-- parses laws in a format like:
--   "double negation: \\neg (\\neg p) = p"
parseLaw :: Parser Law
parseLaw = do name  <- someTill anySingle (symbol ":")
              expr1 <- parseExpr
              _     <- symbol "\\equiv" 
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
