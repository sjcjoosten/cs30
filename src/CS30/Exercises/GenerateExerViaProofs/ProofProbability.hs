{-# LANGUAGE BlockArguments #-}
module CS30.Exercises.GenerateExerViaProofs.ProofProbability where
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           CS30.Exercises.Probability.SolutionChecker
import Control.Monad.Combinators.Expr

-- initial datatypes
data FracExpr 
    = FConst Rational -- the constants 0 and 1
    | FVar Char -- basic evert, such as A, B 
    | FExpr FracExpr FracExpr 
    | FBin MathOp FracExpr FracExpr -- ^ operations
    | AndEvent FracExpr FracExpr  -- ‘and’ \wedge 
    | OrEvent FracExpr FracExpr -- ‘or’ \vee  
    | NegaEvent FracExpr  -- event negation \neg
    | Omega -- evet Omega
    | EmptySet -- emptySet
    | Prob FracExpr -- indicate \frac{}{}
    deriving (Show, Eq)

data Proof = Proof FracExpr [FracStep] deriving (Show, Eq)
type FracStep = (String, FracExpr)

-- datatypes for operations
data MathOp
    = Minus 
    | Divide   -- division
    | Mult   -- multiply 
    | Plus 
    | Frac  -- \frac{}{}
    deriving (Show, Eq)

-- operatable to parse operations
-- based on the operation priority: negaition -> multiplication -> division -> add and minus
operatorTable :: [[Operator Parser FracExpr]]
operatorTable = [[Prefix 
                    (do {_ <- string "\\neg" <|> string "!";
                        lt_spaces;
                        return NegaEvent})], -- for negation
                [mathOp "\\cdot" Mult, mathOp "*" Mult, mathOp "/" Divide],
                [mathOp "+" Plus, mathOp "-" Minus],
                [binary "\\wedge" AndEvent, binary "&" AndEvent, 
                    binary "|" OrEvent,binary "\\vee" OrEvent] -- \wedge and \vee would not show at the same time with add and minus
                ]                                              -- Therefore, tehre is no need to consider their operation priority.      


binary :: String -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL 
                (do{_ <- string name;
                    lt_spaces;
                    return f})

mathOp :: String -> MathOp -> Operator Parser FracExpr
mathOp str opin = binary str (FBin opin)