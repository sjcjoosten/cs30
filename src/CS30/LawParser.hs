module CS30.LawParser where
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
-- import           Data.Functor.Identity

type Parser   = Parsec Void String
data Law      = Law LawName Equation
type LawName  = String
type Equation = (Expr,Expr)
data MathExpr = MathConst Integer 
              | MathVar String
              | Fact MathExpr            -- factorial
              | Divide MathExpr MathExpr  -- division
              | Mult  MathExpr MathExpr  -- multiply two MathExpr's 
              | Expon MathExpr MathExpr  -- set first MathExpr to the second MathExpr power
              | Add MathExpr MathExpr
              | Sub MathExpr MathExpr
              | Neg MathExpr
              deriving Show
data Expr     = Var String | Const Integer | Op Opr [Expr] deriving (Eq,Show)
data Opr      = Multiplication | Division | Addition | Subtraction
              | Exponentiation | Factorial | Negate deriving (Eq,Show)
data Proof    = Proof Expr [(String, Expr)]

-- law :: Parser Law
-- law = do {name <- upto ':';
--           eqn <- equation; return (Law name eqn)}

-- upto :: Char -> Parser String upto c
-- = Parser (\s ->
-- let (xs,ys) = break (==c) s in
--                         if null ys then []
--                         else [(xs,tail ys)])

instance Show Law where 
  showsPrec _ (Law name (e1,e2))
    = showString name . 
      showString ": " . 
      shows e1 . 
      showString " = " . 
      shows e2

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
          
          --let's hope we never divide by zero lol
          , "Dividing Zero: 0 / a = 0"
          , "Multiplicative Inverse: a / a = 1"
          , "Distributive Law: a - (b + c) = a - b - c"
          , "Distributive Law: a - (b - c) = a - b + c" ]

getProofLengthN :: Int -> [Law] -> (Expr -> Law -> [Expr] ) -> Expr -> [Proof]
getProofLengthN 0 _ _ e = [Proof e []]
getProofLengthN n laws fnc e
  = [ Proof e ((lawName,e'):steps)
    | (Law lawName eqn) <- laws
    , e' <- fnc e (Law lawName eqn)
    , (Proof _ steps) <- getProofLengthN (n-1) laws fnc e']
  -- where go exp = case [(lawName,res) | (Law lawName eqn) <- laws, res <- fnc exp (Law lawName eqn)] of 
  --                 [] -> []
  --                 (step@(lname,res):_) -> step:go res

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

-- parse spaces (used w/ symbol and lexeme)
-- spaceConsumer :: Parser ()
-- spaceConsumer = L.space spaces empty empty 

-- based on Drill 6.2 scaffold
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
    [binary "\\cdot" Mult, binary "*" Mult, binary "" Mult, binary "/" Divide] , 
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

-- support needed for ≥, ≤, <, >
-- will add those once we get this working for =
parseLaw :: Parser Law
parseLaw = do lawName <- parseUntil ':'
              -- _ <- string ":"
              lhs <- parseExpr
              _ <- string "="
              rhs <- parseExpr
              return (Law lawName (lhs,rhs))

parseUntil :: Char -> Parser String
parseUntil c = (do _ <- satisfy (== c)
                   return  []) <|>
               (do c1 <- satisfy (const True)
                   rmd <- parseUntil c
                   return (c1:rmd)
               )
