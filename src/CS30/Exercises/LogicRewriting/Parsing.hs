module CS30.Exercises.LogicRewriting.Parsing where

import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import           Control.Monad.Combinators.Expr
import           Data.Void
import           Data.Functor.Identity

data Law = Law {lawName :: String, lawEqn :: Equation}
    deriving Show
type Equation = (Expr,Expr)

data Expr = Var Char          -- variable like 'p' or 'q' 
          | Const Bool        -- either true or false
          | Bin Op Expr Expr  -- binary operations (and,or,implies)
          | Neg Expr          -- negation (\neg <expr>)
    deriving Eq

data Op = And     -- <expr> \wedge <expr> 
        | Or      -- <expr> \vee <expr>
        | Implies -- <expr> \Rightarrow <expr>
    deriving Eq

-- precedence of operators, for printing parens
opPrec :: Op -> Int
opPrec (And)     = 3
opPrec (Or)      = 2
opPrec (Implies) = 1

-- symbols of operators, for printing
opSymb :: Op -> String
opSymb (And)     = "\\ \\wedge\\ "
opSymb (Or)      = "\\ \\vee\\ "
opSymb (Implies) = "\\ \\Rightarrow\\ "

-- display an Expr as a string (to be used with FMath)
instance Show Expr where
    showsPrec _ (Var v)        = showChar v
    showsPrec _ (Const b)      = if b then showString "\\text{true}" 
                                 else showString "\\text{false}"
    showsPrec p (Neg e)        = showParenLATEX (p >= 4) (showString "\\neg " . showsPrec 4 e)
    showsPrec p (Bin op e1 e2) = showParenLATEX (p >= prec)
                                   (showsPrec prec e1 . showString symb . showsPrec prec e2)
                                 where prec = opPrec op 
                                       symb = opSymb op

-- evaluates an expression containing only variables 'p', 'r', 'q'
evalExpr :: Expr -> [Bool]
evalExpr (Var v)        = case v of
                           'p' -> [True,True,True,True,False,False,False,False]
                           'q' -> [True,True,False,False,True,True,False,False]
                           'r' -> [True,False,True,False,True,False,True,False]
                           _   -> error "expression contains unrecognized variable (not p,q, or r)"
evalExpr (Const b)      = replicate 8 b
evalExpr (Neg e)        = [not b | b <- evalExpr e]
evalExpr (Bin op e1 e2) = [(perform op) b1 b2 
                          | (b1,b2) <- zip (evalExpr e1) (evalExpr e2)]
                          where perform (And)     = (&&)
                                perform (Or)      = (||)
                                perform (Implies) = (\x y -> not x || y)

-- check whether both sides of a law evaluate to the same thing
checkLaw :: Law -> Bool
checkLaw (Law _ (e1,e2)) = evalExpr e1 == evalExpr e2

-- same as showParen, but with "\left" and "\right" too
showParenLATEX :: Bool -> ShowS -> ShowS
showParenLATEX b p = if b 
                     then showString "\\left(" . p . showString "\\right)" 
                     else p

-- contains all the laws that we use (after parsing)
lawStrings :: [String]
lawStrings = [
        "Negation Law: p || !p = true",
        "Negation Law: p && !p = false",
        "Idempotent Law: p || p = p",
        "Idempotent Law: p && p = p",
        "Double Negation Law: !(!p) = p",
        "Definition of True: !false = true",
        "Definition of False: !true = false",
        "Identity Law: p || false = p",
        "Identity Law: p && true = p",
        "Domination Law: p && false = false",
        "Domination Law: p || true = true",
        "Implication Law: p => q = !p || q",
        "De Morgan's Law: !(p && q) = !p || !q",
        "De Morgan's Law: !(p || q) = !p && !q"
    ]

-- parse all the laws into our Law data structure, and
-- also include commutative versions of some laws 
--  (with the expressions flipped)
-- and the Commutativity/Associativity laws necessary
--  to apply all the laws 
laws :: [Law]
laws = lawsAndFlips normalLaws ++ assocAndComm normalLaws
       where normalLaws = map prsLaw lawStrings
             prsLaw x = case parse parseLaw "" x of
                           Left s  -> error (show s) 
                           Right l -> l

-- returns True if the given expression is a commutative 
-- (and we assume also associative) binary expression
commutes :: Expr -> Bool
commutes (Bin op _ _)
 = case op of
     And -> True
     Or  -> True
     Implies -> False
commutes _ = False

-- a law needs a commutative version if the lhs is a  
-- commutative binary expression where both sides of 
-- the binary operation are not the same 
needsComm :: Expr -> Bool
needsComm expr 
  = case expr of 
      (Bin op e1 e2) -> commutes (Bin op e1 e2) && e1 /= e2
      _                -> False

-- return the given list of laws, with commutative versions of the laws
-- (with the expressions flipped) interspersed where necessary
-- 
-- e.g. we will have two negation laws: "p || !p = true" and 
--  "!p || p = true" immediately next to one another in the list, 
--  so the order of applying laws remains the same
lawsAndFlips :: [Law] -> [Law]
lawsAndFlips lws = [law {lawEqn=eqn} | law <- lws, 
                                       eqn <- flips (lawEqn law)] 
                    where flips (e1, e2) = if needsComm e1
                                           then [(e1,e2), (flipExpr e1, flipExpr e2)]
                                           else [(e1,e2)]
                          flipExpr (Bin op e1 e2) = Bin op e2 e1
                          flipExpr expr = expr

-- generate a list of  associativity and commutativity laws 
-- which should only be applied when needed to apply the given list of laws
-- 
-- e.g. we rewrite "(p || r) || p" as "(r || p) || p" using commutativity
-- and then "r || (p || p)" using associativity, so we can apply idempotency
assocAndComm :: [Law] -> [Law]
assocAndComm lws 
    = (concatMap comms binLawExprs) ++ (concatMap assoc binLawExprs)
      where binLawExprs = [e1 | law <- lws, 
                                let (e1,_) = lawEqn law, 
                                commutes e1]
            -- ^ we only need associativity/commutativity for laws that 
            -- match either an "and" or an "or" expression 
            comms (Bin op e1 e2) = [Law "Commutativity" (Bin op (Bin op e1 (Var 'x')) e2, 
                                                         Bin op (Bin op (Var 'x') e1) e2), 
                                    Law "Commutativity" (Bin op e1 (Bin op (Var 'x') e2), 
                                                         Bin op e1 (Bin op e2 (Var 'x')))]
            -- ^ use Var 'x' here to not conflict with any variables in the original law
            comms _ = []
            assoc (Bin op e1 e2) = [Law "Associativity" (Bin op (Bin op (Var 'x') e1) e2, 
                                                         Bin op (Var 'x') (Bin op e1 e2)), 
                                    Law "Associativity" (Bin op e1 (Bin op e2 (Var 'x')), 
                                                         Bin op (Bin op e1 e2) (Var 'x'))]
            assoc _ = []

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
      [binary "\\vee" (Bin Or), binary "||" (Bin Or), 
       binary "\\wedge" (Bin And), binary "&&" (Bin And),
       binary "\\Rightarrow" (Bin Implies), 
       binary "=>" (Bin Implies)]
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
