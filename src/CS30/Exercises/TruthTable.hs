{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.TruthTable where
-- import CS30.Data
-- import CS30.Exercises.Data -- 'exerciseType' function

import Text.Parsec
import Text.Parsec.String

import qualified Data.Map as Map
import Data.List (sort)

-- import Data.Aeson.TH -- for deriveJSON
-- import Debug.Trace

-- myExercise :: ExerciseType
-- myExercise = exerciseType "URLTag" "L?.?"
--                "Category: Short Description"
--                choiceTreeList -- List of problems
--                genQuestion -- present question as 'Exercise'
--                genFeedback -- handle response as 'ProblemResponse'

--------------------------------------Data and Types-------------------------------------------

data Expression
  = LiteralP | LiteralQ | LiteralR | LiteralS 
  | Conjunction Expression Expression
  | Disjunction Expression Expression
  | Implication Expression Expression
  | Negation Expression
  deriving (Show, Eq, Ord)

type Literal = Expression
type TruthTable = ([Expression], [[Bool]])

-- data BoolVal = TrueVal | FalseVal | ErrVal -- TODO : Required in the future

--------------------------------------The parsing bit-------------------------------------------

-- Converts an expression string to an object
parseExpr :: String -> Expression
parseExpr = getExpr . parseExpr'

parseExpr' :: String -> Either ParseError Expression -- Helper
parseExpr' x = parse expr1 "<myparse>" (filter (\c->c/=' ') x)

getExpr :: Either ParseError Expression -> Expression -- TODO : Make exhaustive
getExpr (Right x) = x

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

-- Converts an expression object to a string
showExpr :: Expression -> String
showExpr LiteralP = "P"
showExpr LiteralQ = "Q"
showExpr LiteralR = "R"
showExpr LiteralS = "S"
showExpr (Negation e) = "~" ++ showExpr e
showExpr (Conjunction e1 e2) = "(" ++ showExpr e1 ++ " & " ++ showExpr e2 ++ ")"
showExpr (Disjunction e1 e2) = "(" ++ showExpr e1 ++ " | " ++ showExpr e2 ++ ")"
showExpr (Implication e1 e2) = "(" ++ showExpr e1 ++ " > " ++ showExpr e2 ++ ")"

-- Instantiates literals and evaluates the boolean value of an expression
evalExpr :: Expression -> Map.Map Literal Bool -> Bool
evalExpr (Negation e) literalMapping = not (evalExpr e literalMapping)
evalExpr (Conjunction e1 e2) literalMapping = evalExpr e1 literalMapping && evalExpr e2 literalMapping
evalExpr (Disjunction e1 e2) literalMapping = evalExpr e1 literalMapping || evalExpr e2 literalMapping
evalExpr (Implication e1 e2) literalMapping = not (evalExpr e1 literalMapping && not (evalExpr e2 literalMapping))
evalExpr literal literalMapping = if Map.member literal literalMapping then literalMapping Map.! literal else True

--------------------------------------Truth Table Logic-------------------------------------------

-- This returns the truth table for an expression (Header: [Expression], Body: [[Bool]])
getTruthTable :: Expression -> TruthTable
getTruthTable e = (header, body)
                  where header = flattenExpr e ++ [e]
                        literalMappings = getAllLiteralMappings e
                        body = getTruthTableBody header literalMappings

-- This validates a truth table. Given the allocation of values in literal columns, it checks the rest of the body
isValidTruthTable :: TruthTable -> Bool
isValidTruthTable t = input_body == getTruthTableBody input_header literalMappings
                    where input_header = fst t
                          input_body = snd t
                          literals = filter isLiteral input_header
                          literalMappings = map (\row-> Map.fromList(zip literals (take (length literals) row))) input_body
                          
-- This returns the truth table body given a header and a literal to boolean mapping for every row
getTruthTableBody :: [Expression] -> [Map.Map Literal Bool] -> [[Bool]]
getTruthTableBody header literalMappings = 
  map (\literalMapping -> 
                         map (\headerExp -> 
                                           evalExpr headerExp literalMapping 
                             ) header
      ) literalMappings

--------------------------------------Truth Table Helpers-------------------------------------------

-- This breaks an expression into its components in a bottom up fashion, listing literals first, in sorted order (Header row of truth table)
flattenExpr :: Expression -> [Expression]
flattenExpr e = literals ++ nonLiterals
                where flattened = (dedup . flattenExpr') e
                      literals = sort(filter isLiteral flattened)
                      nonLiterals = filter (not . isLiteral) flattened

flattenExpr' :: Expression -> [Expression] -- Helper
flattenExpr' (Negation e) = flattenExpr' e ++ [e]
flattenExpr' (Conjunction e1 e2) = flattenExpr' e1 ++ flattenExpr' e2 ++ [e1, e2]
flattenExpr' (Disjunction e1 e2) = flattenExpr' e1 ++ flattenExpr' e2 ++ [e1, e2]
flattenExpr' (Implication e1 e2) = flattenExpr' e1 ++ flattenExpr' e2 ++ [e1, e2]
flattenExpr' e = [e]

-- This returns all the combinations of literals with assigned boolean values (One for each row of truth table)
getAllLiteralMappings :: Expression -> [Map.Map Literal Bool]
getAllLiteralMappings e = map (\boolMapping -> Map.fromList(zip literals boolMapping)) boolMappings
                          where literals = getLiteralsExpr e
                                boolMappings = cartesionProductN (length literals)

-- Returns all boolean combinations of length n (One for each row of hte truth table)
cartesionProductN :: Int -> [[Bool]] -- Helper
cartesionProductN 0 = [[]]
cartesionProductN n = [True:c | c <- cp] ++ [False:c | c <- cp]
                      where cp = cartesionProductN (n-1)

-- Gets the sorted literals out of an expression
getLiteralsExpr :: Expression -> [Literal] -- Helper
getLiteralsExpr = filter isLiteral . flattenExpr

dedup :: Eq a => [a] -> [a] -- Helper
dedup [] = []
dedup (x:xs) = if elem x xs then dedup xs else [x] ++ dedup xs

-- Checks whether two expressions are semantically equal (When their parse tree has idential splittings irrespective of  the order of their children)
semanticallyEqual :: Expression -> Expression -> Bool
semanticallyEqual (Conjunction e1 e2) (Conjunction e3 e4) = (semanticallyEqual e1 e3 && semanticallyEqual e2 e4) || (semanticallyEqual e1 e4 && semanticallyEqual e2 e3)
semanticallyEqual (Disjunction e1 e2) (Disjunction e3 e4) = (semanticallyEqual e1 e3 && semanticallyEqual e2 e4) || (semanticallyEqual e1 e4 && semanticallyEqual e2 e3)
semanticallyEqual (Implication e1 e2) (Implication e3 e4) = (semanticallyEqual e1 e3 && semanticallyEqual e2 e4) || (semanticallyEqual e1 e4 && semanticallyEqual e2 e3)
semanticallyEqual (Negation e1) (Negation e2) = semanticallyEqual e1 e2
semanticallyEqual e1 e2 | isLiteral e1 && isLiteral e2 = e1 == e2
                        | otherwise = False

-- Checks if the truth values for two expressions are the same
logicallyEqual :: Expression -> Expression -> Bool
logicallyEqual e1 e2 = sameLiterals && (map (evalExpr e1) alm == map (evalExpr e2) alm)
                       where sameLiterals = sort (getLiteralsExpr e1) == sort (getLiteralsExpr e2)
                             alm = getAllLiteralMappings e1

-- Whether an expression is a pure literal
isLiteral :: Expression -> Bool
isLiteral x = elem x [LiteralP, LiteralQ, LiteralR, LiteralS]

-- Tests

_s :: String
_s = "(P>Q)|(R&S)"

_e :: Expression
_e = parseExpr _s

_t :: TruthTable
_t = getTruthTable _e

{-
Tests:
parseExpr _s == Disjunction (Implication LiteralP LiteralQ) (Conjunction LiteralR LiteralS)
getTruthTable _e == ([LiteralP,LiteralQ,LiteralR,LiteralS,Implication LiteralP LiteralQ,Conjunction LiteralR LiteralS,Disjunction (Implication LiteralP LiteralQ) (Conjunction LiteralR LiteralS)],[[True,True,True,True,True,True,True],[True,True,True,False,True,False,True],[True,True,False,True,True,False,True],[True,True,False,False,True,False,True],[True,False,True,True,False,True,True],[True,False,True,False,False,False,False],[True,False,False,True,False,False,False],[True,False,False,False,False,False,False],[False,True,True,True,True,True,True],[False,True,True,False,True,False,True],[False,True,False,True,True,False,True],[False,True,False,False,True,False,True],[False,False,True,True,True,True,True],[False,False,True,False,True,False,True],[False,False,False,True,True,False,True],[False,False,False,False,True,False,True]])
-}
