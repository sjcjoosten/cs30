{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.ModularArithmetic.ModuloParser where

import Data.Void
import Data.Functor.Identity

import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.Combinators.Expr

--------------------------------------Data and Types-------------------------------------------

type Parser = ParsecT Void String Identity
type ParseError = ParseErrorBundle String Void

data Proof = Proof Expression [ProofStep] | ProofError deriving (Show, Eq)
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
                  deriving (Eq, Ord) -- TODO : Add show?

data Op = Neg | Pow | Mul | Sub | Add | ModEq deriving (Eq, Ord) -- TODO : Add show?

--------------------------------------Characters-------------------------------------------

-- TODO : Replace with special characters wherever applicable
charNeg, charAdd, charSub, charMul, charPow :: Char
charNeg = '-'
charAdd = '+'
charSub = '-'
charMul = '*'
charPow = '^'

charOpen, charClose, charEquals, charColon :: Char
charOpen = '('
charClose = ')'
charEquals = '='
charColon = ':'

stringMod, stringImplies, stringCongruent :: String
stringMod = "mod"
stringImplies = "implies"
stringCongruent = "\\equiv_p" -- ≡

--------------------------------------Parser Functions-------------------------------------------

parseLaw :: Bool -> String -> Law
parseLaw isFixed str = getLaw (parse (parserLaw isFixed) "<myparse>" (filter (\y->y/=' ') str))
                       where getLaw (Right law) = law
                             getLaw _ = LawError

parserLaw :: Bool -> Parser Law
parserLaw isFixed = do _name <- someTill anySingle (char charColon)
                       _lhs  <- parserExpression isFixed
                       _     <- char charEquals
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
parserParenthesis = between (char charOpen) (char charClose)

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ 
    [Prefix (UnOp Neg <$ char charNeg)],
    [InfixL (BinOp Pow <$ char charPow)],
    [InfixL (BinOp Mul <$ char charMul)],
    [InfixL (BinOp Sub <$ char charSub)],
    [InfixL (BinOp Add <$ char charAdd)],
    [InfixL (BinOp ModEq <$ string stringCongruent)]
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
showSpace = showChar ' '

showOp :: Op -> ShowS
showOp = showString . symb

symb :: Op -> String
symb Neg = [charNeg]
symb Pow = [charPow]
symb Mul = [charMul]
symb Sub = [charSub]
symb Add = [charAdd]
symb ModEq = stringCongruent

instance Show Expression where
    showsPrec _ (Con b) = showString (show b)
    showsPrec _ (Fixed b) = showString [b]
    showsPrec _ (Var v) = showString [v]
    showsPrec p (UnOp Neg e) = showParen (p > 6) (showString [charNeg] . showsPrec (7) e)
    showsPrec p (BinOp op e1 e2)
        = showParen (p >= q) (showsPrec q e1 . showSpace . showOp op . showSpace . showsPrec (q+1) e2)
          where q = prec op
    showsPrec _ ExpressionError = showString "ExpressionError"
    showsPrec _ _ = showSpace

instance Show Op where
    show op = symb op

-- instance Show ProofStep where
--     show (nm, e) = showString ("\n" ++ nm ++ " : " ++ show e)

-- instance Show Proof where
--     show (Proof e steps) = showString ("Expression : " ++ show e ++ "\nProof : " ++ intercalate "\n" (map show steps) )

-------------------------------------Tests----------------------------------------

exp1 :: Expression
exp1 = parseExpression True "d * (-(a ^ b)) + (a * -c)"
