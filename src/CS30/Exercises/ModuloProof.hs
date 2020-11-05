{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.ModuloProof where

import Data.List
import Data.Void
import Data.Functor.Identity
-- import Data.Nubmers.Prime (isPrime)

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

data Law = Law {lawName :: String, lawEq :: Equation, eqType :: EqualityType} | LawError deriving (Show, Eq)
-- TODO : See where the distinction for primes comes into picture 
-- data EqualityType = Regular | ModPrime | ModNonPrime deriving (Show, Eq) 
data EqualityType = Equals | Congruent deriving (Show, Eq)
type Equation = (Expression, Expression)

data Expression = Con Int |
                  Var Char | -- For problem specific variables
                  Fixed Char | -- For variables comming from a law
                  UnOp Op Expression |
                  BinOp Op Expression Expression |
                  ExpressionError
                  deriving (Show, Eq, Ord)

data Op = Neg | Pow | Mul | Sub | Add deriving (Show, Eq, Ord)

------------------------------------------Basic Laws----------------------------------------

modulo_laws :: [String]
modulo_laws = [
               "Law2 : p \\equiv_p 0 (mod p)",
               "Law1 : x ^ (p-1) \\equiv_p 1 (mod p)"
              ]

-- TODO : add artihmetic laws, 0 ,1 , mul, add, power, handle 0 ^ 0
arithmetic_laws :: [String]
arithmetic_laws = [
                   "Law1 : x * 0 = 0",
                   "Law2 : x + 0 = x",
                   "Law3 : x * 1 = x",
                   "Law4 : x * -1 = -x",
                   "Law5 : x ^ 0 = 1",
                   "Law6 : 1 ^ x = 1",
                   "Law7 : x + y = y + x",
                   "Law8 : x * y = y * x",
                   "Law9 : x + (y + z) = (x + y) + z",
                   "Law10 : x * (y * z) = (x * y) * z",
                   "Law11 : x * (y + z) = x * y + x * z",
                   "Law12 : x * (y - z) = x * y - x * z",
                   "Law13 : x ^ (y + z) = x ^ y * x ^ z"
                  ]

---------------------------------Character Definitions-------------------------------------------

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

parseLaw :: Bool -> EqualityType -> String -> Law
parseLaw isFixed lawEqType str = getLaw (parse (parserLaw isFixed) "<myparse>" (filter (\y->y/=' ') str))
                                 where parserLaw = if lawEqType == Equals then parserArithmeticLaw else parserModuloLaw
                                       getLaw (Right law) = law
                                       getLaw _ = LawError

-- "Law : x + (y + z) = (x + y) + z",
parserArithmeticLaw :: Bool -> Parser Law
parserArithmeticLaw isFixed = do _name <- someTill anySingle (char charColon)
                                 _lhs  <- parserExpression isFixed
                                 _     <- char charEquals
                                 _rhs  <- parserExpression isFixed
                                 return Law { lawName = _name, lawEq = (_lhs, _rhs), eqType = Equals }

-- "Law : x ^ (p-1) \\equiv_p 1 (mod p)"
parserModuloLaw :: Bool -> Parser Law
parserModuloLaw isFixed = do _name <- someTill anySingle (char charColon)
                             _lhs  <- parserExpression isFixed
                             _     <- string stringCongruent
                             _rhs  <- parserExpression isFixed
                             _     <- char charOpen
                             _     <- string stringMod
                             _mod  <- parserExpression isFixed
                             _     <- char charClose
                             return Law {lawName = _name, lawEq = (_lhs, _rhs), eqType = Congruent}

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
    [InfixL (BinOp Add <$ char charAdd)]
  ]

-------------------------------------Proofs----------------------------------------

getDerivation :: [Law] -> Expression -> Proof
getDerivation = undefined

getSteps :: Equation -> Expression -> [Expression]
getSteps = undefined

match :: Expression -> Expression -> Substitution
match = undefined

apply :: Substitution -> Expression -> Expression
apply = undefined

matchE :: Expression -> Expression -> Maybe Substitution
matchE = undefined

combineTwoSubsts :: Substitution -> Substitution -> Maybe Substitution
combineTwoSubsts = undefined

lookupInSubst :: String -> [(String, p)] -> p
lookupInSubst = undefined

-- would go into rewrite function
-- Law1 : a = b (mod p) implies a + c = b + c (mod p)
-- Law2 : a = b (mod p) implies c + a = c + b (mod p)
-- Law3 : a = b (mod p) implies a * c = b * c (mod p)
-- Law4 : a = b (mod p) implies c * a = c * b (mod p)
-- Law5 : a = b (mod p) implies a ^ c = b ^ c (mod p)

isPrime :: Int -> Bool -- TODO : Replace
isPrime k = elem k [2,3,5,7,11,13,17,19,23,29,31]

-------------------------------------Tests----------------------------------------

debug :: String
debug = intercalate "\n" (map (show . parseLaw False Equals) arithmetic_laws) ++ "\n" ++ 
        intercalate "\n" (map (show . parseLaw False Congruent) modulo_laws) ++ "\n"
