{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.ModuloProof (debugOut) where

import           CS30.Data
import           CS30.Exercises.Data

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

data Expression = Con Int |
                  Var Char |
                  UnOp Op Expression |
                  BinOp Op Expression Expression |
                  ExpressionError
                  deriving (Show, Eq, Ord)

data Op = Neg | Pow | Mul | Sub | Add deriving (Show, Eq, Ord)

data PreCondition = PreCondition (Expression, Expression) | NoPreCondition deriving (Show, Eq)

data Law = Law { 
                 lawName :: String, 
                 lawCond :: PreCondition,
                 lawLhs  :: Expression, 
                 lawRhs  :: Expression, 
                 lawMod  :: Expression
               } | LawError deriving (Show, Eq)

data Proof = Proof Expression [ProofStep] | ProofError deriving (Show, Eq)
type ProofStep = (String, Expression)
type Substitution = [(String, Expression)]

implicit_law1, implicit_law2, implicit_law3, implicit_law4, implicit_law5 :: String
implicit_law1 = "Law1 : a = b (mod p) implies a + c = b + c (mod p)"
implicit_law2 = "Law2 : a = b (mod p) implies c + a = c + b (mod p)"
implicit_law3 = "Law3 : a = b (mod p) implies a * c = b * c (mod p)"
implicit_law4 = "Law4 : a = b (mod p) implies c * a = c * b (mod p)"
implicit_law5 = "Law5 : a = b (mod p) implies a ^ c = b ^ c (mod p)"

explicit_law1, explicit_law2 :: String
explicit_law1 = "Law1 : x ^ (p-1) = 1 (mod p)"
explicit_law2 = "Law2 : a + p*b = a (mod p)"

---------------------------------Character Definitions-------------------------------------------

-- Operations -- TODO : Replace with special characters
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

stringMod, stringImplies :: String
stringMod = "mod"
stringImplies = "implies"

--------------------------------------Random Expression Generation-------------------------------

-- Takes a list of operations and fit them in between a,b,c,d
-- Example gen_expressions ['*', '+']
-- a + b + c + d
-- a + b + c * d
-- a + b * c + d
-- a + b * c * d
gen_expressions :: String -> [String]
gen_expressions [] = []
gen_expressions oper = [sbst op |op <- op_list]
    where
        op_list = [[i,j,k] | i <- oper, j <- oper, k <- oper]
        sbst x = (concat [[fst a, snd a] | a <- zip "abc" x]) ++ ['d']

-- Randomly get an expression from a choice Tree
get_expression :: String -> ChoiceTree String
get_expression oper = nodes (gen_expressions [charAdd, charMul])

--------------------------------------Parser Functions-------------------------------------------

parseLaw :: Bool -> String -> Law
parseLaw isImplicit str = getLaw (parse parserLaw "<myparse>" (filter (\y->y/=' ') str))
                          where parserLaw = if isImplicit then parserImplicitLaw else parserExplicitLaw
                                getLaw (Right law) = law
                                getLaw _ = LawError

parserImplicitLaw :: Parser Law
parserImplicitLaw = do _name <- someTill anySingle (char charColon)
                       _ex1  <- parserExpression
                       _     <- char charEquals
                       _ex2  <- parserExpression
                       _     <- char charOpen
                       _     <- string stringMod
                       _     <- parserExpression
                       _     <- char charClose
                       _     <- string stringImplies
                       _lhs  <- parserExpression
                       _     <- char charEquals
                       _rhs  <- parserExpression
                       _     <- char charOpen
                       _     <- string stringMod
                       _mod  <- parserExpression
                       _     <- char charClose
                       return Law { lawName = _name, lawCond = PreCondition (_ex1, _ex2), lawLhs = _lhs, lawRhs = _rhs, lawMod = _mod }

parserExplicitLaw :: Parser Law
parserExplicitLaw = do _name <- someTill anySingle (char charColon)
                       _lhs  <- parserExpression
                       _     <- char charEquals
                       _rhs  <- parserExpression
                       _     <- char charOpen
                       _     <- string stringMod
                       _mod  <- parserExpression
                       _     <- char charClose
                       return Law { lawName = _name, lawCond = NoPreCondition, lawLhs = _lhs, lawRhs = _rhs, lawMod = _mod }

parseExpression :: String -> Expression
parseExpression x = getExpression (parse parserExpression "<myparse>" (filter (\y->y/=' ') x))
                    where getExpression (Right law) = law
                          getExpression _ = ExpressionError

parserExpression :: Parser Expression
parserExpression = makeExprParser parserTerm operatorTable

parserTerm :: Parser Expression
parserTerm = Con <$> L.lexeme space L.decimal <|>
             Var <$> L.lexeme space lowerChar <|>
             parserParenthesis parserExpression

parserParenthesis :: Parser a -> Parser a
parserParenthesis = between (char charOpen) (char charClose)

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ 
    [Prefix (UnOp Neg <$ char charNeg)],
    [InfixL (BinOp Pow <$ char charPow)],
    [InfixL (BinOp Mul <$ char charMul)],
    [InfixL (BinOp Sub <$ char charSub)],
    [InfixL (BinOp Add <$ char charAdd)]
  ]

-------------------------------------Modulo Evaluator----------------------------------------

evalExpression :: Expression -> Int -> Maybe Int
evalExpression (Con i) m = modulo (Just i) m
evalExpression (UnOp op e) m = modulo (fnOp op (evalExpression e m) Nothing) m
evalExpression (BinOp op e1 e2) m = modulo (fnOp op (evalExpression e1 m) (evalExpression e2 m)) m
evalExpression _ _ = Nothing

modulo :: Maybe Int -> Int -> Maybe Int
modulo (Just i) m = Just (i `mod` m)
modulo _ _ = Nothing

fnOp :: Op -> Maybe Int -> Maybe Int -> Maybe Int
fnOp Neg (Just x) _ = Just (-x)
fnOp op (Just i1) (Just i2) 
  = Just (opVal i1 i2)
    where opVal = case op of
                  Pow -> (^)
                  Mul -> (*)
                  Sub -> (-)
                  Add -> (+)
fnOp _ _ _ = Nothing

-------------------------------------Tests----------------------------------------

_e :: String
_e = "12-13 * 32 ^4"

debugOut :: String
debugOut = intercalate "\n" (map (show . (parseLaw True)) [implicit_law1, implicit_law2, implicit_law3, implicit_law4, implicit_law5]) ++ "\n" ++ 
           intercalate "\n" (map (show . (parseLaw False)) [explicit_law1, explicit_law2]) ++ "\n" ++ 
           show (parseExpression _e) ++ "\n" ++ 
           show (evalExpression (parseExpression _e) 11)

-------------------------------------Proofs----------------------------------------

-- getDerivation :: [Law] -> ProbExpr -> Proof
-- getDerivation laws e
--  = Proof e (multiSteps e)
--  where multiSteps e'
--         = case [ (lawName law, res)
--                | law <- laws
--                , res <- getStep (lawEq law) e'
--                ] of
--            [] -> []
--            ((nm,e''):_) -> (nm,e'') : multiSteps e''

