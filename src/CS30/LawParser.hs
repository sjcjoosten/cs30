import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
import           Data.Functor.Identity

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

-- parse something between {...}
brackets :: Parser a -> Parser a
brackets = between (symbol "{") (symbol "}")

-- parse something between (...) 
parens :: Parser a -> Parser a          
parens = between (symbol "\\left(") (symbol "\\right)")

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
    -- ^ we allow the literal word "choose" in between two expressions
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

-- parse a fraction, like \frac{4}{2}
parseFrac :: Parser MathExpr
parseFrac = do
  _  <- symbol "\\frac" 
  e1 <- brackets parseMathExpr
  e2 <- brackets parseMathExpr
  return (Divide e1 e2)

-- parses a term (some expression in brackets/parens, a constant alone, or a binom/frac)
parseTerm :: Parser MathExpr
parseTerm = parens parseMathExpr <|> brackets parseMathExpr <|> parseConstant <|> parseVar <|> parseFrac

-- parse a full expression (using makeExprParser)
parseMathExpr :: Parser MathExpr
parseMathExpr =  makeExprParser parseTerm operatorTable


-- parseTerm :: Parser MathExpr
-- parseTerm = parens parseExpr <|> brackets parseExpr <|> parseConstant <|> parseBinom <|> parseFrac

-- parseMathExpr :: Parser MathExpr
-- parseExpr =  makeExprParser parseTerm operatorTable


-- parseExpr = lt_spaces *> makeExprParser termP oprs <* lt_spaces
--   where termP =     parse_paren parseExpr
--                 -- <|> parse_exp
--                 -- <|> parse_fac
--                 -- <|> parse_mult
--                 -- <|> parse_div
--                 -- <|> parse_add
--                 -- <|> parse_sub
--                 <|> (do num <- some digit
--                         lt_spaces
--                         let ans = parseDec num
--                         return (Const ans))
--                 <|> (do v <- some var
--                         lt_spaces
--                         return (Var v))
--                 --  <|>
--                 --  (do _ <- "\\frac"
--                 --      t1 <- termP
--                 --      t2 <- termP
--                 --      lt_spaces
--                 --      return (Op Division [t1,t2]))
--                 --  <|>
--                 --  (do _ <- "\\left"
--                 --      _ <- openBrac
--                 --      lt_spaces
--                 --      e <- rationalExpr
--                 --      _ <- "\\right"
--                 --      _ <- closeBrac
--                 --      lt_spaces
--                 --      return e
--                 --      )
--                 --  <|>
                 
--         openBrac = string "(" <|> string "{"
--         closeBrac = string ")" <|> string "}"
--         parseDec = foldl (\x y -> x*10 + y) 0
--         lt_spaces :: Parser ()
--         lt_spaces = ((string " " <|> string "\\t" <|> string "\\ ") *> lt_spaces)
--                     <|> return ()
--         oprs =  [ [ postfix "!" Factorial]
--                 , [ infixL "^" Exponentiation
--                   , Prefix (return (UnaryOp Negate) <* string "-" <* lt_spaces)] -- negate, exponentiation
--                 , [ infixL "\\cdot" Multiplication -- for LaTeX
--                   , infixL "*" Multiplication -- for ASCII
--                   , infixL "/" Division] -- multiplication, division
--                 , [ infixL "-" Subtraction
--                   , infixL "+" Addition] -- minus and addition
--                ]
--         infixL str opr = InfixL (return (BinOp opr) <* string str <* lt_spaces)
--         postfix name f = Postfix (return (UnaryOp f) <$ string name)
--         parse_paren :: Parser Expr
--         parse_paren = do
--           _ <- openBrac
--           lt_spaces
--           e <- parseExpr
--           _ <- closeBrac
--           lt_spaces
--           return e
--         -- parse_mult :: Parser Expr
--         -- parse_mult = do
--         --   a <- parseExpr
--         --   lt_spaces
--         --   _ <- "\\cdot"
--         --   lt_spaces
--         --   b <- parseExpr
--         --   lt_spaces
--         --   return (Op Multiplication [a,b])
--         -- parse_div :: Parser Expr
--         -- parse_div = do
--         --   a <- parseExpr
--         --   lt_spaces
--         --   _ <- "/"
--         --   lt_spaces
--         --   b <- parseExpr
--         --   lt_spaces
--         --   return (Op Division [a,b])
--         -- parse_add :: Parser Expr
--         -- parse_add = do
--         --   a <- parseExpr
--         --   lt_spaces
--         --   _ <- "+"
--         --   lt_spaces
--         --   b <- parseExpr
--         --   lt_spaces
--         --   return (Op Addition [a,b])
--         -- parse_sub :: Parser Expr
--         -- parse_sub = do
--         --   a <- parseExpr
--         --   lt_spaces
--         --   _ <- "-"
--         --   lt_spaces
--         --   b <- parseExpr
--         --   lt_spaces
--         --   return (Op Subtraction [a,b])
--         -- parse_exp :: Parser Expr
--         -- parse_exp = do
--         --   a <- parseExpr
--         --   lt_spaces
--         --   _ <- "^"
--         --   lt_spaces
--         --   b <- parseExpr
--         --   lt_spaces
--         --   return (Op Exponentiation [a,b])
--         -- parse_fac :: Parser Expr
--         -- parse_fac = do
--         --   a <- parseExpr
--         --   lt_spaces
--         --   _ <- "!"
--         --   lt_spaces
--         --   return (Op Factorial [a])

parseExpr :: Parser Expr
parseExpr = do mex <- parseMathExpr
               return (mathToExpr mex)

-- data MathExpr = MathConst Integer 
--               | MathVar String
--               | Fact MathExpr            -- factorial
--               | Divide MathExpr MathExpr  -- division
--               | Mult  MathExpr MathExpr  -- multiply two MathExpr's 
--               | Expon MathExpr MathExpr  -- set first MathExpr to the second MathExpr power
--               | Add MathExpr MathExpr
--               | Sub MathExpr MathExpr
--               | Neg MathExpr

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
parseLaw = do lawName <- parseUntil ':'
              _ <- string ":"
              lhs <- parseExpr
              _ <- string "="
              rhs <- parseExpr
              return (Law lawName (lhs,rhs))

parseUntil :: Char -> Parser String
parseUntil c = (do _ <- satisfy (== c)
                   return  []) <|>
               (do c <- satisfy (const True)
                   rmd <- parseUntil c
                   return (c:rmd)
               )
