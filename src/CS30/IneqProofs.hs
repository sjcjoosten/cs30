module CS30.IneqProofs where
import CS30.Data
import CS30.Exercises.Data
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L
-- import           Data.Functor.Identity

type Parser     = Parsec Void String
data Law        = Law {lawName :: String, lawEq :: Equation}
--type LawName    = String <- I don't think we ever use this
type Equation   = (Expr,Expr)

data MathExpr  = MathConst Integer 
               | MathVar String
               | Fact MathExpr             -- factorial
               | Divide MathExpr MathExpr  -- division
               | Mult  MathExpr MathExpr   -- multiply two MathExpr's 
               | Expon MathExpr MathExpr   -- set first MathExpr to the second MathExpr power
               | Add MathExpr MathExpr
               | Sub MathExpr MathExpr
               | Neg MathExpr
               deriving Show
data Expr      = Var String | Const Integer | Op Opr [Expr] deriving (Eq,Show)
data Opr       = Multiplication | Division | Addition | Subtraction
               | Exponentiation | Factorial | Negate deriving (Eq,Show)
data Proof     = Proof Expr [ProofStep] deriving Show
type ProofStep = (String, Expr)

type Inequality = (Expr,Ineq,Expr)
data Ineq       = GThan | GEq | LThan | LEq deriving (Eq)
type Implies    = (Inequality,Inequality)
data IneqLaw    = IneqLaw {ineqName :: String, ineqEq :: Implies}

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
  showsPrec _ LThan = showString " < "
  showsPrec _ LEq   = showString " ≤ "

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

ineqLawList :: [String]
ineqLawList = [ "Multiplication for x > 0: x * y > x * z \\Rightarrow y > z"
              , "Multiplication for x > 0: y * x > z * x \\Rightarrow y > z"
              , "Multiplication for x ≥ 0: y > z \\Rightarrow x * y ≥ x * z"
              , "Multiplication for x > 0: y > z \\Rightarrow x * y > x * z"
              , "Multiplication for x > 0: y > z \\Rightarrow y * x > z * x"
              , "Multiplication for x ≥ 0: y > z \\Rightarrow y * x ≥ z * x" ]

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

-- support needed for ≥, ≤, <, >
-- will add those once we get this working for =
parseLaw :: Parser Law
parseLaw = do lawName <- parseUntil ':'
              lhs     <- parseExpr
              _       <- string "="
              rhs     <- parseExpr
              return (Law lawName (lhs,rhs))

parseIneqLaw :: Parser IneqLaw
parseIneqLaw = do 
               ineqName <- parseUntil ':'
               lhs1     <- parseExpr
               ineq1    <- parseUntil '>' <|> parseUntil '≥' <|> parseUntil '<' <|> parseUntil '≤'
               rhs1     <- parseExpr
               _        <- string "\\Rightarrow"
               lhs2     <- parseExpr
               ineq2    <- parseUntil '>' <|> parseUntil '≥' <|> parseUntil '<' <|> parseUntil '≤'
               --ineq2    <- string ">" <|> string "≥" <|> string "<" <|> string "≤"
               rhs2     <- parseExpr
               return (IneqLaw ineqName ((lhs1, ineq1, rhs1), (lhs2, ineq2, rhs2)))


parseUntil :: Char -> Parser String
parseUntil c = (do _ <- satisfy (== c)
                   return  []) <|>
               (do c1 <- satisfy (const True)
                   rmd <- parseUntil c
                   return (c1:rmd)
               )

law1 = Law "Assoc" (Op Addition [Op Addition [Var "X",Var "Y"],Var "Z"], Op Addition [Var "X", Op Addition [Var "Y", Var "Z"]])

-- generateRandEx :: Int -> ChoiceTree Expr
-- generateRandEx = undefined

-- data Law = Law String Equation
-- lawName (Law nm _) = nm
-- lawEq (Law _ eq) = eq
-- data Law = Law {lawName :: String, lawEq :: Equation}

getDerivation :: [Law] -> Expr -> Proof
getDerivation laws e
 = Proof e (multiSteps e)
 where multiSteps e'
        = case [ (lawName law, res)
               | law <- laws
               , res <- getStep (lawEq law) e'
               ] of
           [] -> []
           ((nm,e''):_) -> (nm,e'') : multiSteps e''

-- as example of getStep:
-- (5 - 3) - 1
-- x - y = x + (negate y)
-- lhs: x - y, rhs: x + (negate y)
-- expr: E[(5 - 3) - 1]
-- subst to get: x = 5 - 3, y = 1

getStep :: Equation -> Expr -> [Expr]
getStep (lhs, rhs) expr
  = case matchE lhs expr of
      Nothing -> recurse expr
      Just subst -> [apply subst rhs]
  where recurse (Var _) = []
        recurse (Const _) = []
        recurse (Op o [e1,e2])
          = [Op o [e1', e2] | e1' <- getStep (lhs,rhs) e1] ++
            [Op o [e1, e2'] | e2' <- getStep (lhs,rhs) e2]
        recurse (Op o [e1])
          = [Op o [e1'] | e1' <- getStep (lhs,rhs) e1]
-- 0 + x = x
type Substitution = [(String, Expr)]

matchE :: Expr -> Expr -> Maybe Substitution
matchE (Var nm) expr = Just [(nm,expr)]
matchE (Const i) (Const j) | i == j = Just []
matchE (Const _) _ = Nothing
matchE (Op o1 [e1,e2]) (Op o2 [e3,e4]) | o1 == o2
 = case (matchE e1 e3, matchE e2 e4) of
    (Just s1, Just s2) -> combineTwoSubsts s1 s2
    _ -> Nothing
matchE (Op o1 [e1]) (Op o2 [e2]) | o1 == o2
 = matchE e1 e2
matchE (Op _ _) _ = Nothing

combineTwoSubsts :: Substitution -> Substitution -> Maybe Substitution
combineTwoSubsts s1 s2
  = case and [v1 == v2 | (nm1,v1) <- s1, (nm2,v2) <- s2, nm1 == nm2] of
     True -> Just (s1 ++ s2)
     False -> Nothing

apply :: Substitution -> Expr -> Expr
apply subst (Var nm) = lookupInSubst nm subst
apply subst (Const i) = Const i
apply subst (Op o [e]) = Op o [apply subst e]
apply subst (Op o [e1,e2]) = Op o [(apply subst e1),(apply subst e2)]

lookupInSubst :: String -> [(String, p)] -> p
lookupInSubst nm ((nm',v):rm)
 | nm == nm' = v
 | otherwise = lookupInSubst nm rm
lookupInSubst _ [] = error "Substitution was not complete, or free variables existed in the rhs of some equality"
