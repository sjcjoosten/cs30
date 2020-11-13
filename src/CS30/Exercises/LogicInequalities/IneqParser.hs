module CS30.Exercises.LogicInequalities.IneqParser where

import           Control.Monad.Combinators.Expr
import           Data.Maybe ( catMaybes )
import           Data.Void ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char ( string )
import qualified Text.Megaparsec.Char.Lexer as L

{- defining datas and types -}
type Parser        = Parsec Void String
data Expr          = Var String | Const Integer | Op Opr [Expr] deriving (Eq)  -- | ConstVar String
data Opr           = Multiplication | Division | Addition | Subtraction
                   | Exponentiation | Factorial | Negate deriving (Eq,Show)
data Law           = Law {lawName :: String, lawEq :: Equation}
type Equation      = (Expr,Expr)
data Proof         = Proof Expr [ProofStep] deriving Show
type ProofStep     = (String, Expr)
-- MathExpr for parsing, gets converted to Expr for everything else
data MathExpr      = MathConst Integer 
                   | MathVar String
                   | Fact MathExpr             -- factorial
                   | Divide MathExpr MathExpr  -- division
                   | Mult  MathExpr MathExpr   -- multiply two MathExpr's 
                   | Expon MathExpr MathExpr   -- set first MathExpr to the second MathExpr power
                   | Add MathExpr MathExpr
                   | Sub MathExpr MathExpr
                   | Neg MathExpr
                   deriving Show
-- maybe we can ultimately get rid of these
data IneqLaw       = IneqLaw {ineqName :: String, ineqEq :: Implies}
data IneqProof     = IneqProof Inequality [IneqProofStep]
type IneqProofStep = (String, Inequality)
type Inequality    = (Expr,Ineq,Expr)
data Ineq          = GThan | GEq | EqEq deriving (Eq)
type Implies       = (Inequality,Inequality)

{- defining Show instances -}
instance Show Law where 
  showsPrec _ (Law name (e1,e2))
    = showString name . 
      showString ": " . 
      shows e1 .
      showString " = " . 
      shows e2

instance Show IneqLaw where 
  showsPrec _ (IneqLaw name ((e11,i1,e12),(e21,i2,e22)))
    = showString name . 
      showString ": " . 
      shows e11 . shows i1 . shows e12 .
      showString " \\Rightarrow " . 
      shows e21 . shows i2 . shows e22

instance Show Ineq where
  showsPrec _ GThan = showString " > "
  showsPrec _ GEq   = showString " ≥ "
  showsPrec _ EqEq  = showString " = "

instance Show Expr where
  showsPrec _ (Var s) = showString s
  showsPrec _ (Const n) = showString (show n)
  showsPrec p (Op o [e1]) = case o of
    Factorial -> showParens (p>q) (showsPrec q e1 . showString (symb Factorial))
    Negate -> showParens (p>q) (showString (symb Negate) . showsPrec q e1)
    _ -> showParens (True) (showsPrec q e1)
    where q = prec o
  showsPrec p (Op o [e1,e2]) = showParens (p>q) (showsPrec q e1 . showString (symb o) . showsPrec (q+1) e2)
      where q = prec o
  showsPrec _ (Op _ _) = showString "()"

prec :: Opr -> Int
prec Factorial = 3
prec Exponentiation = 3
prec Negate = 3
prec Multiplication = 2
prec Division = 2
prec Addition = 1
prec Subtraction = 1

symb :: Opr -> String
symb Multiplication = " \\cdot "
symb Division = " / "
symb Addition = " + "
symb Subtraction = " - "
symb Factorial = "!"
symb Exponentiation = "^"
symb Negate = "-"

showParens :: Bool -> (String -> String) -> (String -> String)
showParens True s = showChar '(' . s . showChar ')'
showParens False s = s


stringsToLaw :: [String] -> [Law]
stringsToLaw lst = catMaybes $ map convert lst
  where convert v = case parse parseLaw "" v of
                 Left _ -> Nothing
                 Right l -> Just l


{- Our parser -}

digit :: Parser Integer
digit = do c <- satisfy inRange
           return (toInteger (charToInteger c))
   where charToInteger c = fromEnum c - fromEnum '0'
         inRange c = let r = charToInteger c
                     in r >= 0 && r < 10

-- maybe add support for uppercase letter later
var :: Parser Char
var = do c <- satisfy inRange
         return c
   where charToInteger c = fromEnum c - fromEnum 'a'
         inRange c = let r = charToInteger c
                     in r >= 0 && r < 26

-- maybe add support for uppercase letter later
parseVar :: Parser MathExpr
parseVar = do s <- some var
              return (MathVar s)

spaces :: Parser ()
spaces = many spc >> return ()
 where spc = string " " <|> string "\t" <|> string "\n" <|> string "\r"
             <|> string "\\ " <|> string "~"

-- parse a given specific string, accounting for spaces 
symbol :: String -> Parser String
symbol = lexeme . string

-- parse some lexeme, accounting for spaces
lexeme :: Parser a -> Parser a
lexeme x = spaces *> x <* spaces

-- parse something between (...) 
parens :: Parser a -> Parser a          
parens = between (symbol "(") (symbol ")")

-- parses some integer number alone into MathExpr
parseConstant :: Parser MathExpr
parseConstant = do
  n <- lexeme L.decimal
  return (MathConst n)

-- operator table for use with makeExprParser 
operatorTable :: [[Operator Parser MathExpr]]
operatorTable =
  [ [postfix "!" Fact],
    [prefix "-" Neg],
    [binary "^" Expon] ,
    [binary "\\cdot" Mult, binary "*" Mult, binary "/" Divide] ,
    [binary "+" Add, binary "-" Sub]
  ]

-- helper function for generating an binary infix operator
-- like multiplication or exponentiation
-- based on documentation for Control.Monad.Combinators.Expr
binary :: String -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ symbol name)

-- helper function for postfix operators like factorial
-- also from Control.Monad.Combinators.Expr documentation 
postfix :: String -> (a -> a) -> Operator Parser a
postfix name f = Postfix (f <$ symbol name)

prefix :: String -> (a -> a) -> Operator Parser a
prefix  name f = Prefix (f <$ symbol name)

-- parses a term (some expression in brackets/parens, a constant alone, or a binom/frac)
parseTerm :: Parser MathExpr
parseTerm = (parens parseMathExpr <|> parseConstant <|> parseVar) <* spaces

-- parse a full expression (using makeExprParser)
parseMathExpr :: Parser MathExpr
parseMathExpr = spaces *> makeExprParser parseTerm operatorTable <* spaces

parseExpr :: Parser Expr
parseExpr = do mex <- parseMathExpr
               return (mathToExpr mex)

mathToExpr :: MathExpr -> Expr
mathToExpr (MathVar s) = Var s
mathToExpr (MathConst n) = Const n
mathToExpr (Fact a) = Op Factorial [mathToExpr a]
mathToExpr (Divide a1 a2) = Op Division [mathToExpr a1, mathToExpr a2]
mathToExpr (Mult a1 a2) = Op Multiplication [mathToExpr a1, mathToExpr a2]
mathToExpr (Expon a1 a2) = Op Exponentiation [mathToExpr a1, mathToExpr a2]
mathToExpr (Add a1 a2) = Op Addition [mathToExpr a1, mathToExpr a2]
mathToExpr (Sub a1 a2) = Op Subtraction [mathToExpr a1, mathToExpr a2]
mathToExpr (Neg a) = Op Negate [mathToExpr a]

parseLaw :: Parser Law
parseLaw = do lname <- parseUntil ':'
              lhs     <- parseExpr
              _       <- string "="
              rhs     <- parseExpr
              return (Law lname (lhs,rhs))

parseUntil :: Char -> Parser String
parseUntil c = (do _ <- satisfy (== c)
                   return  []) <|>
               (do c1 <- satisfy (const True)
                   rmd <- parseUntil c
                   return (c1:rmd)
               )
