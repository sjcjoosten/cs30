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

data Expr = Var Char          -- variable like 'p' or 'q' 
          | Const Bool        -- either true or false
          | And Expr Expr     -- <expr> \wedge <expr> 
          | Or Expr Expr      -- <expr> \vee <expr>
          | Implies Expr Expr -- <expr> \Rightarrow <expr>
          | Neg Expr          -- negation (\neg <expr>)
    deriving (Eq, Show)

type Parser = ParsecT Void String Identity

lawStrings :: [String]
lawStrings = [
        "Double Negation Law: \\neg (\\neg p) \\equiv p",
        "Identity Law: p \\vee false \\equiv p",
        "Identity Law: p \\wedge true \\equiv q",
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

laws :: [Law]
laws = map prsLaw lawStrings
       where prsLaw x = case parse parseLaw "" x of
                           Left _  -> error ""
                           Right l -> l

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

-- GENERATING PROOF --

data Proof = Proof Expr [Step] deriving Show
type Step = (String, Expr) -- law name and resulting expression 

-- 
getDerivation :: [Law] -> Expr -> Proof
getDerivation laws e = Proof e (manyStep laws e)

-- 
manyStep :: [Law] -> Expr -> [Step]
manyStep laws e = if null steps then []
                  -- ^ change the condition here to reflect negation??
                  else step : manyStep laws (snd step)
                  where rws expr = [(name,e') | Law name eqn <- laws,
                                                e' <- rewrites eqn e,
                                                e' /= e]
                        steps = rws e
                        step = head steps

-- 
rewrites :: Equation -> Expr -> [Expr]
rewrites (lhs, rhs) e = 
    case matchExpr lhs e of
        []         -> recurse e
        (sub:subs) -> [apply s rhs | s <- (sub:subs)]
    where recurse (Var _)         = []
          recurse (Const _)       = []
          recurse (Neg e)         = [Neg e' | e' <- rewrites (lhs,rhs) e]
          -- TODO: refactor data structure with some sort of BinOp 
          recurse (And e1 e2)     = [And e1' e2 | e1' <- rewrites (lhs, rhs) e1] ++
                                    [And e1 e2' | e2' <- rewrites (lhs, rhs) e2]
          recurse (Or e1 e2)      = [Or e1' e2 | e1' <- rewrites (lhs, rhs) e1] ++
                                    [Or e1 e2' | e2' <- rewrites (lhs, rhs) e2]
          recurse (Implies e1 e2) = [Implies e1' e2 | e1' <- rewrites (lhs, rhs) e1] ++
                                    [Implies e1 e2' | e2' <- rewrites (lhs, rhs) e2]

type Subst = [(Char, Expr)]

-- 
matchExpr :: Expr -> Expr -> [Subst]
matchExpr (Var v) e = [ [(v,e)] ]
matchExpr (Const i) (Const j) | i == j = [[]]
matchExpr (Const _) _ = []
matchExpr (Neg e1) (Neg e2) = matchExpr e1 e2
matchExpr (Neg _) _  = []
matchExpr (And e1 e2) (And e3 e4)
    = combine (matchExpr e1 e3) (matchExpr e2 e4)
matchExpr (And _ _) _  = []
matchExpr (Or e1 e2) (Or e3 e4) 
    = combine (matchExpr e1 e3) (matchExpr e2 e4)
matchExpr (Or _ _) _  = []
matchExpr (Implies e1 e2) (Implies e3 e4) 
    = combine (matchExpr e1 e3) (matchExpr e2 e4)
matchExpr (Implies _ _) _  = []

-- e.g. trying to matchExpr "p ∧ q" to "p ∧ (p ∨ q)"
--  we first matchExpr the left and right hand sides of the binary op "∧"
--  resulting in [[('p', Var p)]] and [[('q', And (Var p) (Var q))]]
--  we want to have [[('p', Var p), ('q', And (Var p) (Var q))]]
-- 
--  alternately... combine ["a"] ["bc"] = ["ab", "ac"]
combine :: [Subst] -> [Subst] -> [Subst]
combine xs ys = [x ++ y | x <- xs, y <- ys, compatible x y] 
                where compatible x y = and [e1 == e2 
                                           | (v1,e1) <- x, 
                                             (v2,e2) <- y,
                                             v1 == v2]

-- 
apply :: Subst -> Expr -> Expr
apply subst (Var v)         = lookupInSubst v subst
apply subst (Const b)       = Const b
apply subst (Neg e)         = Neg (apply subst e)
apply subst (And e1 e2)     = And (apply subst e1) (apply subst e2)
apply subst (Or e1 e2)      = Or (apply subst e1) (apply subst e2)
apply subst (Implies e1 e2) = Implies (apply subst e1) (apply subst e2)

-- 
lookupInSubst :: Char -> Subst -> Expr
lookupInSubst v ((v', expr):rm)
    | v == v'   = expr
    | otherwise = lookupInSubst v rm
lookupInSubst _ [] = error "Substitution was not complete"
