module CS30.Exercises.LogicExpr.Datatypes where

import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import Data.Either

type Parser = Parsec Void String
data LogicExpr 
    = Con Bool
    | Var Char
    | Bin LogicOp LogicExpr LogicExpr
    | Neg LogicExpr
    deriving (Show, Eq)

data LogicOp
    = And
    | Or
    | Imply
    deriving (Show, Eq)

data Law = Law LawName Equation deriving (Show)
type LawName = String
type Equation = (LogicExpr,LogicExpr)

type Substitution = [(VarName,LogicExpr)]
type VarName = Char

data Calculation = Calc LogicExpr [Step] deriving (Show)

type Step = (LawName, LogicExpr)

emptySub :: Substitution
emptySub = []

unitSub :: VarName -> LogicExpr -> Substitution
unitSub v e = [(v,e)]
