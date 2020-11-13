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

type Parser = ParsecT Void String Identity
type ParseError = ParseErrorBundle String Void

data Proof = Proof Expression [ProofStep] | ProofError deriving (Eq)
type ProofStep = (String, Expression)
type Substitution = [(String, Expression)]

data Law = Law {lawName :: String, lawEq :: Equation} | LawError deriving (Show, Eq)
type Equation = (Expression, Expression)

data Expression = Con Int |
                  Var Char | -- For problem specific variables
                  Fixed Char | -- For variables comming from a law
                  UnOp Op Expression |
                  BinOp Op Expression Expression |
                  ExpressionError
                  deriving (Eq, Ord)

data Op = Neg | Pow | Mul | Sub | Add | ModEq deriving (Eq, Ord)

--------------------------------------Parser Functions-------------------------------------------

parseLaw :: Bool -> String -> Law
parseLaw isFixed str = getLaw (parse (parserLaw isFixed) "<myparse>" (filter (\y->y/=' ') str))
                       where getLaw (Right law) = law
                             getLaw _ = LawError

parserLaw :: Bool -> Parser Law
parserLaw isFixed = do _name <- someTill anySingle (string ":")
                       _lhs  <- parserExpression isFixed
                       _     <- string "="
                       _rhs  <- parserExpression isFixed
                       return Law { lawName = _name, lawEq = (_lhs, _rhs)}

parseExpression :: Bool -> String -> Expression
parseExpression isFixed x = getExpression (parse (parserExpression isFixed) "<myparse>" (filter (\y->y/=' ') x))
                            where getExpression (Right law) = law
                                  getExpression _ = ExpressionError

parserExpression :: Bool -> Parser Expression
parserExpression isFixed = makeExprParser (parserTerm isFixed) operatorTable

parserTerm :: Bool -> Parser Expression
parserTerm isFixed = Con <$> L.lexeme space L.decimal <|>
                     (if isFixed then Fixed else Var) <$> L.lexeme space lowerChar <|>
                     parserParenthesis (parserExpression isFixed)

parserParenthesis :: Parser a -> Parser a
parserParenthesis = between (string "(") (string ")")

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

prec :: Op -> Int
prec ModEq = 1
prec Add = 2
prec Sub = 3
prec Mul = 4
prec Pow = 5
prec Neg = 6

showSpace :: ShowS
showSpace = showString " "

showOp :: Op -> ShowS
showOp = showString . symb

symb :: Op -> String
symb Neg = "-"
symb Pow = "^"
symb Mul = "\\cdot"
symb Sub = "-"
symb Add = "+"
symb ModEq = "\\equiv_{p}"

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

instance Show Op where
    show op = symb op

instance Show Proof where
    show (Proof e steps) = "Expression:\n" ++ show e ++ "\n\nProof:\n " ++ intercalate "\n" (map show steps)
    show ProofError = "ProofError"
