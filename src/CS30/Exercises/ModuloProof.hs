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

------------------------------------------Implicit Laws----------------------------------------

modulo_laws :: [Law]
modulo_laws = map (parseLaw False Congruent)
              [
               "Law2 : p \\equiv_p 0 (mod p)",
               "Law1 : x ^ (p-1) \\equiv_p 1 (mod p)"
              ]

-- TODO : add artihmetic laws, 0 ,1 , mul, add, power, handle 0 ^ 0
arithmetic_laws :: [Law]
arithmetic_laws = map (parseLaw False Equals)
                  [
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

getDerivation :: Int -> [Law] -> Expression -> Proof
getDerivation steps laws e
 = Proof e (multiSteps steps e)
 where multiSteps 0 _ = []
       multiSteps steps' e'
        = case [ (lawName law, res)
               | law <- laws
               , res <- getStep (lawEq law) e'
               ] of
                   [] -> []
                   ((nm,e''):_) -> (nm,e'') : multiSteps (steps'-1) e''

-- would go into rewrite function
-- Law1 : a = b (mod p) implies a + c = b + c (mod p)
-- Law2 : a = b (mod p) implies c + a = c + b (mod p)
-- Law3 : a = b (mod p) implies a * c = b * c (mod p)
-- Law4 : a = b (mod p) implies c * a = c * b (mod p)
-- Law5 : a = b (mod p) implies a ^ c = b ^ c (mod p)
getStep :: Equation -> Expression -> [Expression]
getStep (lhs, rhs) expr
  = case matchE lhs expr of
      Nothing -> recurse expr
      Just subst -> [apply subst rhs]
  where recurse (Var _) = []
        recurse (Con _) = []
        recurse (Fixed _) = []
        recurse (BinOp op e1 e2)
          = [BinOp op e1' e2 | e1' <- getStep (lhs,rhs) e1] ++
            [BinOp op e1 e2' | e2' <- getStep (lhs,rhs) e2]
        recurse (UnOp op e1)
          = [UnOp op e1' | e1' <- getStep (lhs,rhs) e1]
        recurse (ExpressionError) = []

matchE :: Expression -> Expression -> Maybe Substitution
matchE (Var nm) expr = Just [([nm],expr)]
matchE (Con i) (Con j) | i == j = Just []
matchE (Con _) _ = Nothing
matchE (Fixed i) (Fixed j) | i == j = Just [] -- TODO : Confirm
matchE (Fixed _) _ = Nothing -- TODO : Confirm
matchE (BinOp op1 e1 e2) (BinOp op2 e3 e4) | op1 == op2
 = case (matchE e1 e3, matchE e2 e4) of
    (Just s1, Just s2) -> combineTwoSubsts s1 s2
    _ -> Nothing
matchE (BinOp _ _ _) _ = Nothing
matchE (UnOp Neg e1) (UnOp Neg e2) = matchE e1 e2
matchE (UnOp _ _) _ = Nothing
matchE ExpressionError _ = Nothing

combineTwoSubsts :: Substitution -> Substitution -> Maybe Substitution
combineTwoSubsts s1 s2
  = case and [v1 == v2 | (nm1,v1) <- s1, (nm2,v2) <- s2, nm1 == nm2] of
     True -> Just (s1 ++ s2)
     False -> Nothing

apply :: Substitution -> Expression -> Expression
apply subst (Var nm) = lookupInSubst [nm] subst
apply _ (Con i) = Con i
apply _ (Fixed i) = Fixed i -- TODO : definitely not, change
apply subst (UnOp op e) = UnOp op (apply subst e)
apply subst (BinOp op e1 e2) = BinOp op (apply subst e1) (apply subst e2)
apply _ ExpressionError = ExpressionError

lookupInSubst :: String -> [(String, p)] -> p
lookupInSubst nm ((nm',v):rm)
 | nm == nm' = v
 | otherwise = lookupInSubst nm rm
lookupInSubst _ [] = error "Substitution was not complete, or free variables existed in the rhs of some equality"

isPrime :: Int -> Bool -- TODO : Replace
isPrime k = elem k [2,3,5,7,11,13,17,19,23,29,31]

-------------------------------------Shows----------------------------------------

showExpression :: Expression -> String
showExpression (Con i) = show i
showExpression (Var c) = [c]
showExpression (Fixed c) = [c]
showExpression (BinOp op e1 e2) = [charOpen] ++ showExpression e1 ++ [showOp op] ++ showExpression e2 ++ [charClose]
showExpression (UnOp op e) = [showOp op] ++ [charOpen] ++ showExpression e ++ [charClose]
showExpression ExpressionError = "ExpressionError!"

showOp :: Op -> Char
showOp Neg = charNeg
showOp Pow = charPow
showOp Mul = charMul
showOp Sub = charSub
showOp Add = charAdd

showProof :: Proof -> String
showProof ProofError = "ProofError"
showProof (Proof e steps) = "Expression: " ++ showExpression e ++ "\bProof: " ++ showProofStep steps

showProofStep :: [ProofStep] -> String
showProofStep [] = ""
showProofStep ((nm, e):rest) = "\n" ++ nm ++ [charColon] ++ showExpression e ++ showProofStep rest

-------------------------------------Tests----------------------------------------

debug :: String
debug = intercalate "\n" (map show arithmetic_laws) ++ "\n" ++ 
        intercalate "\n" (map show modulo_laws) ++ "\n"

exp1 :: Expression
exp1 = parseExpression False "((0+1)+2)+3"

prf1 :: Proof
prf1 = getDerivation 5 arithmetic_laws exp1
