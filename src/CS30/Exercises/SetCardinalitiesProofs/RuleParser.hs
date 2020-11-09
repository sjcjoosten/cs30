{-# LANGUAGE TemplateHaskell #-}
module CS30.Exercises.SetCardinalitiesProofs.RuleParser where
import           Control.Monad.Combinators.Expr
import           Data.Functor.Identity
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Data.Aeson as JSON
import           Data.Aeson.TH
import           Data.Char
import qualified Text.Megaparsec.Char.Lexer as L

type Parser = Parsec Void String
data Symb = Add 
         | Sub 
         | Mult
         | Intersection 
         | Union 
         | Powerset 
         | Cardinality
         | Cartesian
         | Expon
         | Setminus 
         deriving (Show, Eq)

data Expr = Val Integer | Var Char | Op Symb [Expr] deriving (Show, Eq)

data Law = Law {lawName :: String, lawEquation :: Equation} deriving (Show)

type Equation = (Expr, Expr)  -- (left,right)

$(deriveJSON defaultOptions ''Symb)
$(deriveJSON defaultOptions ''Expr)

laws :: [Law]
laws = map lawFromString lawStrings
   where
      lawStrings = [ -- all given Rules
         "Inclusion-Exclusion: |A \\cup B| = |A| + |B| - |A \\cap B|",      
         "Cardinality of cartesian Product: |A \\times B| = |A| \\cdot |B|",
         "Powerset Cardinality: |\\P(A)| = 2^{|A|}",
         "Setminus Cardinality: |A \\setminus B| = |A| - |A \\cap B|"
         ]

lawFromString :: String -> Law
lawFromString s = case (parse (pLaw parseExpr) "" s) of 
               Right law -> law
               Left e -> error (errorBundlePretty e)

exprFromString :: String -> Expr
exprFromString s = case (parse parseExpr "" s) of 
               Right expr -> expr
               Left e -> error (errorBundlePretty e)

pLaw :: Parser Expr -> Parser Law
pLaw pExpr = 
   do lawNm <- parseUntil ':' <*spaces
      lhs <- pExpr
      _ <- string "=" <*spaces
      rhs <- pExpr
      return (Law lawNm (lhs, rhs))

parseUntil :: MonadParsec e s f => Token s -> f [Token s]
parseUntil c 
   =  (do _ <- satisfy (== c) 
          return []) <|>
      (do ch <- satisfy (const True) 
          rmd <- parseUntil c 
          return (ch:rmd))

-- operator table for use with makeExprParser 
operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ 
     -- Number operations
     [binary "^" Expon],
     [binary "\\cdot" Mult],
     [binary "+" Add, binary "-" Sub],

   --   -- Set operations
     [binary "\\times" Cartesian],
     [binary "\\cup" Union],
     [binary "\\cap" Intersection],
     [binary "\\setminus" Setminus]
  ]

binary :: String -> Symb -> Operator (ParsecT Void String Data.Functor.Identity.Identity) Expr
binary name symb = InfixL ((\lhs rhs -> Op symb [lhs,rhs]) <$ symbol name)


-- parses some integer number alone into MathExpr
-- parseConstant :: Parser Expr
-- parseConstant = do
--   n <- lexeme L.decimal
--   return (Const n)

--next few functions taken from Donia and Bennetts Exercise:

-- parse spaces (used w/ symbol and lexeme)
spaceConsumer :: Parser ()
spaceConsumer = L.space spaces empty empty 

-- based on Drill 6.2 scaffold
spaces :: Parser ()
spaces = many spc >> return ()
 where spc = string " " <|> string "\t" <|> string "\n" <|> string "\r"
             <|> string "\\ " <|> string "~"

-- parse a given specific string, accounting for spaces 
symbol :: String -> Parser String
symbol = unspace . string

unspace :: Parser a -> Parser a
unspace p = spaces *> p <* spaces

-- parse something between {...}
brackets :: Parser a -> Parser a
brackets = between (symbol "{") (symbol "}")

-- parse something between |...|
pipes :: Parser a -> Parser a
pipes = between (symbol "|") (symbol "|")

-- parse something between {...}
parseCardinality :: Parser Expr
parseCardinality = do
   x <- pipes parseExpr
   return (Op Cardinality [x])


powerset :: Parser a -> Parser a
powerset = between (symbol "\\P(") (symbol ")")

parsePowerset :: Parser Expr
parsePowerset = do
   x <- powerset parseExpr
   return (Op Powerset [x])


parseVar :: Parser Expr
parseVar = Var <$> satisfy isCapLetter <* spaces
   where
      isCapLetter c = elem c ['A'..'Z']

parseInt :: Parser Expr
parseInt = do
   num <- some digit
   return (Val (read num))

digit :: Parser Char
digit = satisfy isMyDigit
            where isMyDigit x = elem x ['0'..'9']

parseTerm :: Parser Expr
parseTerm =  brackets parseExpr <|> parseCardinality <|> parsePowerset <|> parseVar <|> parseInt

parseExpr :: Parser Expr
parseExpr =  makeExprParser parseTerm operatorTable


-- Haskell types to Latex String conversions:

lawToLatex :: Law -> String
lawToLatex (Law name eq) = name ++ ": " ++ eqToLatex eq

eqToLatex :: Equation -> String
eqToLatex (lhs,rhs) = exprToLatex lhs ++ " = " ++ exprToLatex rhs

exprToLatex :: Expr -> String
exprToLatex (Var v) = [v]
exprToLatex (Val v) = show v
-- unary operators
exprToLatex (Op Cardinality [e]) = "|" ++ exprToLatex e ++ "|"
exprToLatex (Op Powerset [e]) = "\\P(" ++ exprToLatex e ++")"
-- binary operators with brackets
exprToLatex (Op Expon [e1,e2]) = "(" ++ exprToLatex e1 ++ ")^{" ++ exprToLatex e2 ++ "}"
-- binary operators without brackets
exprToLatex (Op symb [e1,e2]) = exprToLatex e1 ++ " " ++ symbLookup symb ++ " " ++ exprToLatex e2
exprToLatex e = error ("Invalid expression: " ++ show e)

symbLookup :: Symb -> String
symbLookup s 
   | s == Add           = "+"
   | s == Sub           = "-"
   | s == Mult          = "*"
   | s == Intersection  = "\\cap"
   | s == Union         = "\\cup"
   | s == Cartesian     = "\\times"
   | s == Expon         = "^"
   | s == Setminus      = ""
   | otherwise          = error ("Invalid symbol: " ++ show s)


-- Given Rules:
-- |A \\cup B| = |A| + |B| - |A \\cap B|
-- |A \\times B| = |A| \\cdot |B|
-- |\\P(A)| = 2^{|A|}
-- |A \\setminus B| = |A| - |A \\cap B|
