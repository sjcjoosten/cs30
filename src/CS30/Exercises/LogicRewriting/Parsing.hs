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
    deriving Eq

-- display an Expr as a string (to be used with FMath)
instance Show Expr where
    -- showsPrec :: Int -> Expr -> ShowS
    showsPrec _ (Var v)         = showChar v
    showsPrec _ (Const b)       = if b then showString "\\text{true}" 
                                 else showString "\\text{false}"
    showsPrec p (Neg e)         = showParenLATEX (p >= 4) (showString "\\neg " . showsPrec 4 e)
    showsPrec p (And e1 e2)     = showParenLATEX (p > 3)
                                   (showsPrec 3 e1 . showString "\\ \\wedge\\ " . showsPrec 3 e2)
    showsPrec p (Or e1 e2)      = showParenLATEX (p > 2)
                                   (showsPrec 2 e1 . showString "\\ \\vee\\ " . showsPrec 2 e2)
    showsPrec p (Implies e1 e2) = showParenLATEX (p > 1)
                                   (showsPrec 1 e1 . showString "\\ \\Rightarrow\\ " . showsPrec 1 e2)                               

-- evaluates an expression containing only variables 'p', 'r', 'q'
evalExpr :: Expr -> [Bool]
evalExpr (Var v)         = case v of
                            'p' -> [True,True,True,True,False,False,False,False]
                            'q' -> [True,True,False,False,True,True,False,False]
                            'r' -> [True,False,True,False,True,False,True,False]
                            _   -> error "expression contains unrecognized variable (not p,q, or r)"
evalExpr (Const b)       = replicate 8 b
evalExpr (Neg e)         = [not b | b <- evalExpr e]
evalExpr (And e1 e2)     = [b1 && b2 | (b1,b2) <- zip (evalExpr e1) (evalExpr e2)]
evalExpr (Or e1 e2)      = [b1 || b2 | (b1,b2) <- zip (evalExpr e1) (evalExpr e2)]
evalExpr (Implies e1 e2) = [not b1 || b2 | (b1,b2) <- zip (evalExpr e1) (evalExpr e2)]

checkLaw :: Law -> Bool
checkLaw (Law _ (e1,e2)) = evalExpr e1 == evalExpr e2

-- 
showParenLATEX :: Bool -> ShowS -> ShowS
showParenLATEX b p = if b 
                     then showString "\\left(" . p . showString "\\right)" 
                     else p

-- contains all the laws that we use (after parsing)
lawStrings :: [String]
lawStrings = [
        "Double Negation Law: !(!p) = p",
        "Identity Law: p || false = p",
        "Identity Law: p && true = p",
        "Domination Law: p && false = false",
        "Domination Law: p || true = true",
        "Idempotent Law: p || p = p",
        "Idempotent Law: p && p = p",
        "Implication Law: p => q = !p || q",
        "De Morgan's Law: !(p && q) = !p || !q",
        "De Morgan's Law: !(p || q) = !p && !q",
        "Negation Law: p || !p = true",
        "Negation Law: p && !p = false",
        "Definition of True: !false = true",
        "Definition of False: !true = false"
    ]

-- parse all the laws into our Law data structure
laws :: [Law]
laws = map prsLaw lawStrings
       where prsLaw x = case parse parseLaw "" x of
                           Left s  -> error (show s) 
                           Right l -> l

-- just the (unique) names of the laws
lawNames :: [String]
lawNames = uniques [name | (Law name _) <- laws]
           where uniques [] = []
                 uniques (x:xs) = if x `elem` xs then uniques xs
                                  else x:(uniques xs)

type Parser = ParsecT Void String Identity

-- parses laws in a format like:
--   "double negation: \\neg (\\neg p) = p"
parseLaw :: Parser Law
parseLaw = do name  <- someTill anySingle (symbol ":")
              expr1 <- parseExpr
              _     <- symbol "\\equiv" <|> symbol "="
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
    [ [prefix "\\neg" Neg, prefix "!" Neg],
      [binary "\\vee" Or, binary "||" Or, 
       binary "\\wedge" And, binary "&&" And,
       binary "\\Rightarrow" Implies, binary "=>" Implies]
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
