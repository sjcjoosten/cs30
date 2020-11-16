{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.ModularArithmetic.ModuloParser where

import Data.List
import Data.Void
import Data.Functor.Identity

import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.Combinators.Expr

--------------------------------------Data and Types-------------------------------------------

-- These follow the staandard data types used in class

type Parser = ParsecT Void String Identity
type ParseError = ParseErrorBundle String Void

data Proof = Proof Expression [ProofStep] | ProofError deriving (Eq)
type ProofStep = (String, Expression)
type Substitution = [(String, Expression)]

data Law = Law {lawName :: String, lawEq :: Equation} | LawError deriving (Show, Eq)
type Equation = (Expression, Expression)

data Expression = Con Int | -- Integer constants
                  Var Char | -- For problem specific variables
                  Fixed Char | -- For variables comming from a law
                  UnOp Op Expression |
                  BinOp Op Expression Expression |
                  ExpressionError
                  deriving (Eq, Ord)

data Op = Neg | Pow | Mul | Sub | Add | ModEq deriving (Eq, Ord)

--------------------------------------Parser Functions-------------------------------------------

{-
The "isFixed" parameter has been used throughout the parser functions
If its true, then the string is parsed as a "Given" law, where the variables are problem specific and become Fixed
If its false, then the string is parsed as a generic law, where the "Given" law, where the variables are place holders and become Var
-}

-- Parses a law
parseLaw :: Bool -> String -> Law
parseLaw isFixed str = getLaw (parse (parserLaw isFixed) "<myparse>" (filter (\y->y/=' ') str))
                       where getLaw (Right law) = law
                             getLaw _ = LawError

-- Parser for a law
parserLaw :: Bool -> Parser Law
parserLaw isFixed = do _name <- someTill anySingle (string ":")
                       _lhs  <- parserExpression isFixed
                       _     <- string "="
                       _rhs  <- parserExpression isFixed
                       return Law { lawName = _name, lawEq = (_lhs, _rhs)}

-- Parser for a expression
parserExpression :: Bool -> Parser Expression
parserExpression isFixed = makeExprParser (parserTerm isFixed) operatorTable

-- Parser for a term
parserTerm :: Bool -> Parser Expression
parserTerm isFixed = Con <$> L.lexeme space L.decimal <|>
                     (if isFixed then Fixed else Var) <$> L.lexeme space lowerChar <|>
                     parserParenthesis (parserExpression isFixed)

-- Parser for a parenthesis enclosed entity
parserParenthesis :: Parser a -> Parser a
parserParenthesis = between (string "(") (string ")")

-- Operation table to specify operator mapping and precedence
operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ 
    [Prefix (UnOp Neg <$ string "-")],
    [InfixL (BinOp Pow <$ string "^")],
    [InfixL (BinOp Mul <$ string "\\cdot")],
    [InfixL (BinOp Sub <$ string "-")],
    [InfixL (BinOp Add <$ string "+")],
    [InfixL (BinOp ModEq <$ string "\\equiv_{p}")]
  ]

-------------------------------------Shows----------------------------------------

-- Operator precedence
prec :: Op -> Int
prec ModEq = 1
prec Add = 2
prec Sub = 3
prec Mul = 4
prec Pow = 5
prec Neg = 6

-- Shows space
showSpace :: ShowS
showSpace = showString " "

-- Shows operation
showOp :: Op -> ShowS
showOp = showString . symb

-- String mapping of an operation
symb :: Op -> String
symb Neg = "-"
symb Pow = "^"
symb Mul = "\\cdot"
symb Sub = "-"
symb Add = "+"
symb ModEq = "\\equiv_{p}"

-- Implements Show for an Expression
instance Show Expression where
    showsPrec _ (Con b) = showString (show b)
    showsPrec _ (Fixed b) = showString [b]
    showsPrec _ (Var v) = showString [v]
    showsPrec p (UnOp Neg e) = showParen (p > 6) (showString "-" . showsPrec (7) e)
    showsPrec p (BinOp Pow e1 e2)
        = showParen (p >= q) (showsPrec q e1 . showSpace . showOp Pow . showString "{" . showsPrec (q+1) e2 . showString "}")
          where q = prec Pow
    showsPrec p (BinOp op e1 e2)
        = showParen (p >= q) (showsPrec q e1 . showSpace . showOp op . showSpace . showsPrec (q+1) e2)
          where q = prec op
    showsPrec _ ExpressionError = showString "ExpressionError"
    showsPrec _ _ = showSpace

-- Implements Show for an Operation
instance Show Op where
    show op = symb op

-- Implements Show for a Proof
instance Show Proof where
    show (Proof e steps) = "Expression:\n" ++ show e ++ "\n\nProof:\n " ++ intercalate "\n" (map show steps)
    show ProofError = "ProofError"
