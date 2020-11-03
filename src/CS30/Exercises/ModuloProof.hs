{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.ModuloProof (debugOut) where

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
                  Inv Expression |
                  Pow Expression Expression |
                  Mul Expression Expression |
                  Add Expression Expression |
                  Sub Expression Expression |
                  Mod Expression Expression |
                  ExpressionError
                  deriving (Show, Eq, Ord)

data Proof = Proof [ExplicitLaw] deriving (Show, Eq)
data ImplicitLaw = ImplicitLaw String Equation Equation | ImplicitLawError deriving (Show, Eq)
data ExplicitLaw = ExplicitLaw String Equation | ExplicitLawError deriving (Show, Eq)
type Equation = (Expression, Expression)

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
charInv, charAdd, charSub, charMul, charPow :: Char
charInv = '~'
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

--------------------------------------Parser Functions-------------------------------------------

parseImplicitLaw :: String -> ImplicitLaw
parseImplicitLaw x = getImplicitLaw (parse parserImplicitLaw "<myparse>" (filter (\y->y/=' ') x))
                     where getImplicitLaw (Right law) = law
                           getImplicitLaw _ = ImplicitLawError

parserImplicitLaw :: Parser ImplicitLaw
parserImplicitLaw = do name  <- someTill anySingle (char charColon)
                       expr1 <- parserExpression
                       _     <- char charEquals
                       expr2 <- parserExpression
                       _     <- char charOpen
                       _     <- string stringMod
                       expr3 <- parserExpression
                       _     <- char charClose
                       _     <- string stringImplies
                       expr4 <- parserExpression
                       _     <- char charEquals
                       expr5 <- parserExpression
                       _     <- char charOpen
                       _     <- string stringMod
                       expr6 <- parserExpression
                       _     <- char charClose
                       return (ImplicitLaw name (expr1, Mod expr2 expr3) (expr4, Mod expr5 expr6))

parseExplicitLaw :: String -> ExplicitLaw
parseExplicitLaw x = getExplicitLaw (parse parserExplicitLaw "<myparse>" (filter (\y->y/=' ') x))
                     where getExplicitLaw (Right law) = law
                           getExplicitLaw _ = ExplicitLawError

parserExplicitLaw :: Parser ExplicitLaw
parserExplicitLaw = do name  <- someTill anySingle (char charColon)
                       expr1 <- parserExpression
                       _     <- char charEquals
                       expr2 <- parserExpression
                       _     <- char charOpen
                       _     <- string stringMod
                       expr3 <- parserExpression
                       _     <- char charClose
                       return (ExplicitLaw name (expr1, Mod expr2 expr3))

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
    [InfixL (Pow <$ char charPow)],
    [Prefix (Inv <$ char charInv)],
    [InfixL (Mul <$ char charMul)],
    [InfixL (Sub <$ char charSub)],
    [InfixL (Add <$ char charAdd)]
  ]

-------------------------------------Modulo Evaluator----------------------------------------

evalExpr :: Expression -> Int -> Maybe Int
evalExpr (Con i) m = modulo (Just i) m
evalExpr (Inv e) m = moduloInverse (evalExpr e m) m
evalExpr (Add e1 e2) m = modulo (evalExpr e1 m `fnAdd` evalExpr e2 m) m
evalExpr (Mul e1 e2) m = modulo (evalExpr e1 m `fnMul` evalExpr e2 m) m
evalExpr (Pow e1 (Con i)) m = modulo (evalExpr e1 m `fnPow` i) m
evalExpr _ _ = Nothing

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

-------------------------------------Tests----------------------------------------

debugOut :: String
debugOut = intercalate "\n" (map (show . parseImplicitLaw) [implicit_law1, implicit_law2, implicit_law3, implicit_law4, implicit_law5]) ++ "\n" ++
           intercalate "\n" (map (show . parseExplicitLaw) [explicit_law1, explicit_law2])
