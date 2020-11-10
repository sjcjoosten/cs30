module CS30.Exercises.LogicInequalities.IneqParser where

import           Control.Monad.Combinators.Expr
import           Data.Maybe ( catMaybes )
import           Data.Void ( Void )
import           Text.Megaparsec
import           Text.Megaparsec.Char ( string )
import qualified Text.Megaparsec.Char.Lexer as L

-- defining datas and types
type Parser        = Parsec Void String

-- MathExpr for parsing, converted to Expr for everything else
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

data Expr          = Var String | Const Integer | Op Opr [Expr] deriving (Eq,Show)
data Opr           = Multiplication | Division | Addition | Subtraction
                   | Exponentiation | Factorial | Negate deriving (Eq,Show)
  
data Law           = Law {lawName :: String, lawEq :: Equation}
type Equation      = (Expr,Expr)

data IneqLaw       = IneqLaw {ineqName :: String, ineqEq :: Implies}
data Proof         = Proof Expr [ProofStep] deriving Show
type ProofStep     = (String, Expr)
data IneqProof     = IneqProof Inequality [IneqProofStep]
type IneqProofStep = (String, Inequality)
type Inequality    = (Expr,Ineq,Expr)
data Ineq          = GThan | GEq | EqEq deriving (Eq)
type Implies       = (Inequality,Inequality)

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

{- laws to be used in our proofs -}

-- some of these are duplicated, ex: a * 0 = 0 and 0 * a = 0
lawList :: [String]
lawList = [ "Commutative of Addition: a + b = b + a"
          , "Additive Identity: a + 0 = a"
          , "Additive Identity: 0 + a = a"
          , "Additive Inverse: a - a = 0"
          , "Additive Association: a + (b + c) = (a + b) + c"
          , "Multiplicative Identity: a * 1 = a"
          , "Multiplicative Identity: 1 * a = a"
          , "Multiplicative Association: a * (b * c) = (a * b) * c"
          , "Multiplication Times 0: 0 * a = 0"
          , "Multiplication Times 0: a * 0 = 0"
          , "Dividing Zero: 0 / a = 0"
          , "Multiplicative Inverse: a / a = 1"
          , "Distributive Law: a - (b + c) = a - b - c"
          , "Distributive Law: a - (b - c) = a - b + c" ]

ineqLawList :: [String]
ineqLawList = [ --"Multiplication for x > 0: x * y > x * z \\Rightarrow y > z"    -- hold off for now
              --, "Multiplication for x > 0: y * x > z * x \\Rightarrow y > z"    -- hold off for now
                "Multiplication for x ≥ 0: y > z \\Rightarrow x * y ≥ x * z"      -- done
              , "Multiplication for x > 0: y > z \\Rightarrow x * y > x * z"      -- done
              , "Multiplication for x > 0: y > z \\Rightarrow y * x > z * x"      -- done
              , "Multiplication for x ≥ 0: y > z \\Rightarrow y * x ≥ z * x"      -- done
              , "Addition: x > y \\Rightarrow x + z > y + z"
              , "Division for x > 0 and z > 0: x < y \\Rightarrow z / x > z / y"
              , "Division for x > 0 and z > 0: x > y \\Rightarrow z / x < z / y"
              , "Division for x > 0 and z ≥ 0: x < y \\Rightarrow z / x ≥ z / y"
              , "Division for x > 0 and z ≥ 0: x > y \\Rightarrow z / x ≤ z / y"
              , "Numbers: 1 > 0" -- idk what to do about this last one but it's needed
              ]

stringsToLaw :: [String] -> [Law]
stringsToLaw lst = catMaybes $ map convert lst
  where convert v = case parse parseLaw "" v of
                 Left _ -> Nothing
                 Right l -> Just l

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
