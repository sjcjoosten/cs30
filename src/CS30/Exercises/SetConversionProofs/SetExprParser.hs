{-# LANGUAGE TemplateHaskell #-}

{-
authors: Donia Tung & Mikio Obuchi 

COSC 69.14, 20F
Group Assignment 2
-}

module CS30.Exercises.SetConversionProofs.SetExprParser where
import           Data.Functor.Identity
import           Data.Void

import Data.Aeson.TH 

import           Text.Megaparsec
import           Text.Megaparsec.Char -- readFile
import qualified Text.Megaparsec.Char.Lexer as L
import           Control.Monad.Combinators.Expr

import Debug.Trace


-- datatype that we parse all expressions into 
data SetExpr = Var String -- single variable
              | SetBuilder SetExpr -- set builder expression
              | Power SetExpr   -- powerset
              | Cap SetExpr SetExpr     -- cap operation, intersection
              | Cup SetExpr SetExpr     -- cup operation, union
              | SetMinus  SetExpr SetExpr  -- set difference
              | Wedge SetExpr SetExpr   -- and, intersection
              | Vee SetExpr SetExpr     -- or, union
              | In  SetExpr      -- element of 
              | NotIn SetExpr   -- not an element of
              | Subset SetExpr -- subset of
              deriving (Show, Eq)
$(deriveJSON defaultOptions ''SetExpr)

type Parser = ParsecT Void String Identity

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
brackets = between (symbol "\\left\\{") (symbol "\\right\\}")

-- parse something between (...) 
parens :: Parser a -> Parser a          
parens = between (symbol "\\left(") (symbol "\\right)")

-- parse something between (...) 
exprParens :: Parser a -> Parser a          
exprParens = between (symbol "(") (symbol ")")

-- parses some variable alone into SetExpr
parseConstant :: Parser SetExpr
parseConstant = do
  n <- lexeme L.charLiteral
  return (Var [n])

-- operator table for use with makeExprParser 
operatorTable :: [[Operator Parser SetExpr]] -- order matters! 
operatorTable =

  [ [binary "\\in" (const In), binary "\\notin" (const NotIn)], 
    [prefix "\\P" Power], 
    [binary "\\subseteq" (const Subset)],

    [binary "\\cap" Cap, binary "\\cup" Cup], 
    [binary "\\setminus" SetMinus],
    [binary "\\wedge" Wedge, binary "\\vee" Vee] 
  ]

-- helper function for generating an binary infix operator
-- based on documentation for Control.Monad.Combinators.Expr
binary :: String -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ symbol name)

-- helper function for generating a prefix operator
-- based on documentation for Control.Monad.Combinators.Expr
prefix :: String -> (a -> a) -> Operator (ParsecT Void String Identity) a
prefix  name f = Prefix (f <$ symbol name)

-- helper function to parse until a certain character
parseUntil :: Char -> Parser String 
parseUntil c
  = (do _ <- satisfy (== c)
        return []) <|> 
    (do accum <- satisfy (const True) 
        rmd <- parseUntil c 
        return (accum:rmd) 
        )

-- parse an expression in set builder notation 
parseSetBuilder :: Parser SetExpr
parseSetBuilder 
  = brackets (do
               _ <- symbol "e|"
               remander <-  parseExpr
               return (SetBuilder remander))


-- parses a term (some expression in parens, a constant alone, or an expression in set builder notation)
parseTerm :: Parser SetExpr
parseTerm = parens parseExpr <|> exprParens parseExpr  <|> parseSetBuilder  <|> parseConstant 

-- parse a set expression (using makeExprParser)
parseExpr :: Parser SetExpr
parseExpr =  makeExprParser parseTerm operatorTable

