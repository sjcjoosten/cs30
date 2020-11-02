{-# OPTIONS_GHC -Wall #-}

module ModuloProof (debugOut) where

import Data.List
import Data.Void
import Data.Functor.Identity

import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.Combinators.Expr

--------------------------------------Data and Types-------------------------------------------

type Parser = ParsecT Void String Identity -- parsing strings in this file
type ParseError = ParseErrorBundle String Void -- corresponding error type

data Expression = Var Int |
                  Inv Expression |
                  Pow Expression Expression |
                  Mul Expression Expression |
                  Add Expression Expression |
                  ExpressionError
                  deriving (Show, Eq, Ord)

---------------------------------Character Definitions-------------------------------------------

-- Operations -- TODO : Replace with special characters
charInv, charAdd, charMul, charPow :: Char
charInv = '~'
charAdd = '+'
charMul = '*'
charPow = '^'

-- Parenthesis
charOpen, charClose :: Char
charOpen = '('
charClose = ')'

--------------------------------------Parser Functions-------------------------------------------

-- THE MOST IMPORTANT ONE :)
-- Converts string to an expression
parseExpr :: String -> Expression
parseExpr x = getExpr (parse parserExpression "<myparse>" (filter (\c->c/=' ') x))

getExpr :: Either ParseError Expression -> Expression
getExpr (Right e) = e
getExpr _ = ExpressionError

showExpr :: Expression -> String -- TODO : Remove redundant parenthesis?
showExpr ExpressionError = "Parse Error !"
showExpr (Var i) = show i
showExpr (Inv e) = [charInv] ++ [charOpen] ++ showExpr e ++ [charClose]
showExpr (Add e1 e2) = [charOpen] ++ showExpr e1 ++ [charAdd] ++ showExpr e2 ++ [charClose]
showExpr (Mul e1 e2) = [charOpen] ++ showExpr e1 ++ [charMul] ++ showExpr e2 ++ [charClose]
showExpr (Pow e1 e2) = showExpr e1 ++ [charPow] ++ showExpr e2

-------------------------------------Megaparsec stuff----------------------------------------

parserExpression :: Parser Expression
parserExpression = makeExprParser parserTerm operatorTable

parserTerm :: Parser Expression
parserTerm = Var <$> L.lexeme space L.decimal <|>
             parserParenthesis parserExpression

parserParenthesis :: Parser a -> Parser a
parserParenthesis = between (char charOpen) (char charClose)

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ 
    [InfixL (Pow <$ char charPow)],
    [Prefix (Inv <$ char charInv)],
    [InfixL (Mul <$ char charMul)],
    [InfixL (Add <$ char charAdd)]
  ]

-------------------------------------Modulo Evaluator----------------------------------------

modulo :: Maybe Int -> Int -> Maybe Int
modulo Nothing _ = Nothing
modulo (Just i) m = Just (i `mod` m)

moduloInverse :: Maybe Int -> Int -> Maybe Int
moduloInverse Nothing _ = Nothing
moduloInverse (Just i) m = elemIndex 1 [(i * d) `mod` m  | d <- [0 .. m-1] ]

fnAdd :: Maybe Int -> Maybe Int -> Maybe Int
fnAdd Nothing _ = Nothing
fnAdd _ Nothing = Nothing
fnAdd (Just i1) (Just i2) = Just (i1 + i2)

fnMul :: Maybe Int -> Maybe Int -> Maybe Int
fnMul Nothing _ = Nothing
fnMul _ Nothing = Nothing
fnMul (Just i1) (Just i2) = Just (i1 * i2)

fnPow :: Maybe Int -> Int -> Maybe Int
fnPow Nothing _ = Nothing
fnPow (Just i1) i2 = Just (i1 ^ i2)

evalExpr :: Expression -> Int -> Maybe Int
evalExpr (Var i) m = modulo (Just i) m
evalExpr (Inv e) m = moduloInverse (evalExpr e m) m
evalExpr (Add e1 e2) m = modulo (evalExpr e1 m `fnAdd` evalExpr e2 m) m
evalExpr (Mul e1 e2) m = modulo (evalExpr e1 m `fnMul` evalExpr e2 m) m
evalExpr (Pow e1 (Var i)) m = modulo (evalExpr e1 m `fnPow` i) m
evalExpr _ _ = Nothing

-------------------------------------Tests----------------------------------------

_input :: String
_input = "12+~16^4*37*(534*12+23)"

_parsed :: Expression
_parsed = parseExpr _input

_shown :: String
_shown = showExpr _parsed

_modnum :: Int
_modnum = 11

_eval :: Maybe Int
_eval = evalExpr _parsed _modnum

-- Print this to validate all functions
debugOut :: String
debugOut = "Input \t\t: " ++ _input ++ 
           "\nParsed \t\t: " ++ show _parsed ++
           "\nShown \t\t: " ++ _shown ++
           "\nEval mod "++ show _modnum ++"\t: " ++ show _eval
