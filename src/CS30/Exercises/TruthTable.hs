{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.TruthTable where
-- import CS30.Data
-- import CS30.Exercises.Data -- 'exerciseType' function
-- import Data.List
-- import qualified Data.Map as Map -- handle response
-- import Data.Aeson.TH -- for deriveJSON
-- import Debug.Trace
import Text.Parsec
import Text.Parsec.String

-- myExercise :: ExerciseType
-- myExercise = exerciseType "URLTag" "L?.?"
--                "Category: Short Description"
--                choiceTreeList -- List of problems
--                genQuestion -- present question as 'Exercise'
--                genFeedback -- handle response as 'ProblemResponse'

data Expression
  = LiteralP | LiteralQ | LiteralR | LiteralS 
  | Conjunction Expression Expression
  | Disjunction Expression Expression
  | Implication Expression Expression
  | Negation Expression
  deriving (Show, Eq)

-- data BoolVal = TrueVal | FalseVal | ErrVal -- TODO : Required in the future

expr1 :: Parser Expression
expr1 = chainl1 expr2 op
  where op = Disjunction <$ char '|'
         <|> Conjunction <$ char '&'
         <|> Implication <$ char '>'
      --    <|> Negation char '~' -- TODO : Handle negation

expr2 :: Parser Expression
expr2 = LiteralP <$ char 'P' 
   <|> LiteralQ <$ char 'Q' 
   <|> LiteralR <$ char 'R' 
   <|> LiteralS <$ char 'S'
   <|> expr3 expr1

expr3 :: Parser a -> Parser a
expr3 = between (char '(') (char ')')

parseBoolExp :: String -> Either ParseError Expression
parseBoolExp x = parse expr1 "<myparse>" (filter (\c->c/=' ') x)

getExpr :: Either ParseError Expression -> Expression -- TODO : Make exhaustive
getExpr (Right x) = x

eval :: Expression -> [Bool] -> Bool
eval LiteralP b = b !! 0
eval LiteralQ b = b !! 1
eval LiteralR b = b !! 2
eval LiteralS b = b !! 3
eval (Negation e) b = not (eval e b)
eval (Conjunction e1 e2) b = eval e1 b && eval e2 b
eval (Disjunction e1 e2) b = eval e1 b || eval e2 b
eval (Implication e1 e2) b = not (eval e1 b && not (eval e2 b))

-- Example

strExpr :: String
strExpr = "(P>Q)|(R&S)"

exprObj :: Expression
exprObj = (getExpr.parseBoolExp) strExpr

exprEval :: Bool
exprEval = eval exprObj [True, False, False, True]

{-
Test:
(getExpr.parseBoolExp) "(P>Q)|(R&S)" == Disjunction (Implication LiteralP LiteralQ) (Conjunction LiteralR LiteralS)
eval ((getExpr.parseBoolExp) "(P>Q)|(R&S)") [True, False, False, True] == False
-}