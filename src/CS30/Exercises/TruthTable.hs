{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.TruthTable where
import Text.Parsec
import Text.Parsec.String
import qualified Data.Map as Map

import CS30.Data
import CS30.Exercises.Data -- 'exerciseType' function
import qualified Data.Map as Map -- handle response
import Data.Aeson.TH -- for deriveJSON
import Debug.Trace


data TruthEx = TruthEx

$(deriveJSON defaultOptions ''TruthEx)

truthEx :: ExerciseType
truthEx = exerciseType "URLTag" "L?????.??????"
          "Logic: Complete a TruthTable"
          [Node TruthEx] -- List of problems
          genQuestion -- present question as 'Exercise'
          genFeedback -- handle response as 'ProblemResponse'

genQuestion :: TruthEx -> Exercise -> Exercise
genQuestion _ ex = ex{eQuestion=[FTable [[Header (FText ['P']), Header (FText ['Q']), Header (FText "~P"), Header (FText "~Q"), Header (FText "~PΛ~Q")],
                                         [Cell (FText ['T']), Cell (FText ['T']), Cell (FFieldMath "blank1"), Cell (FText ['F']), Cell (FText ['F'])],
                                         [Cell (FText ['T']), Cell (FText ['F']), Cell (FText ['F']), Cell (FText ['T']), Cell (FText ['F'])],
                                         [Cell (FText ['F']), Cell (FText ['T']), Cell (FText ['T']), Cell (FFieldMath "blank2"), Cell (FText ['F'])],
                                         [Cell (FText ['F']), Cell (FText ['F']), Cell (FText ['T']), Cell (FText ['T']), Cell (FText ['T'])]   
                                        ]]}

genFeedback :: TruthEx -> Map.Map String String -> ProblemResponse -> ProblemResponse
genFeedback _ mStrs resp = if mStrs == (Map.fromList [("blank1", ['F']), ("blank2", ['F'])]) -- Solution will be generated
                            then markCorrect $ resp{prFeedback=[FText "Solution is correct!"]} else error "Solution is incorrect"

                                

--------------------------------------Data and Types-------------------------------------------

data Expression
  = LiteralP | LiteralQ | LiteralR | LiteralS 
  | Conjunction Expression Expression
  | Disjunction Expression Expression
  | Implication Expression Expression
  | Negation Expression
  deriving (Show, Eq, Ord)

type Literal = Expression

-- data BoolVal = TrueVal | FalseVal | ErrVal -- TODO : Required in the future

--------------------------------------The parsing bit-------------------------------------------

-- Converts an expression string to an object
parseExpr :: String -> Expression
parseExpr = getExpr . parseExpr'

parseExpr' :: String -> Either ParseError Expression -- Helper
parseExpr' x = parse expr1 "<myparse>" (filter (\c->c/=' ') x)

getExpr :: Either ParseError Expression -> Expression -- TODO : Make exhaustive
getExpr (Right x) = x

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

--------------------------------------Truth Table Logic-------------------------------------------

-- This returns the truth table for an expression (Header: [Expression], Body: [[Bool]])
getTruthTable :: Expression -> ([Expression], [[Bool]])
getTruthTable e = (header, body)
                  where header = flattenExpr e ++ [e]
                        literalMappings = getAllLiteralMappings e
                        body = map (\literalMapping -> 
                                                      map (\headerExp -> 
                                                                        evalExpr headerExp literalMapping 
                                                          ) header
                                   ) literalMappings

-- This breaks an expression into its components in a bottom up fashion (Header row of truth table)
flattenExpr :: Expression -> [Expression]
flattenExpr = dedup . flattenExpr'

flattenExpr' :: Expression -> [Expression] -- Helper
flattenExpr' LiteralP = [LiteralP]
flattenExpr' LiteralQ = [LiteralQ]
flattenExpr' LiteralR = [LiteralR]
flattenExpr' LiteralS = [LiteralS]
flattenExpr' (Negation e) = flattenExpr' e ++ [e]
flattenExpr' (Conjunction e1 e2) = flattenExpr' e1 ++ flattenExpr' e2 ++ [e1, e2]
flattenExpr' (Disjunction e1 e2) = flattenExpr' e1 ++ flattenExpr' e2 ++ [e1, e2]
flattenExpr' (Implication e1 e2) = flattenExpr' e1 ++ flattenExpr' e2 ++ [e1, e2]

-- This returns all the combinations of literals with assigned boolean values (One for each row of truth table)
getAllLiteralMappings :: Expression -> [Map.Map Literal Bool]
getAllLiteralMappings e = map (\boolMapping -> Map.fromList(zip literals boolMapping)) boolMappings
                          where literals = getLiteralsExpr e
                                boolMappings = cartesionProductN (length literals)

cartesionProductN :: Int -> [[Bool]] -- Helper
cartesionProductN 1 = [[True],[False]]
cartesionProductN n = [[True] ++ c | c <- cp] ++ [[False] ++ c | c <- cp]
                      where cp = cartesionProductN (n-1)

getLiteralsExpr :: Expression -> [Literal] -- Helper
getLiteralsExpr = getLiteralsExpr' . flattenExpr

getLiteralsExpr' :: [Expression] -> [Literal] -- Helper
getLiteralsExpr' [] = []
getLiteralsExpr' (LiteralP : xs) = [LiteralP] ++ getLiteralsExpr' xs
getLiteralsExpr' (LiteralQ : xs) = [LiteralQ] ++ getLiteralsExpr' xs
getLiteralsExpr' (LiteralR : xs) = [LiteralR] ++ getLiteralsExpr' xs
getLiteralsExpr' (LiteralS : xs) = [LiteralS] ++ getLiteralsExpr' xs
getLiteralsExpr' (_ : xs) = getLiteralsExpr' xs

dedup :: Eq a => [a] -> [a] -- Helper
dedup [] = []
dedup (x:xs) = if elem x xs then dedup xs else [x] ++ dedup xs

-- Tests

_strExpr :: String
_strExpr = "(P>Q)|(R&S)"

_exprObj :: Expression
_exprObj = parseExpr _strExpr

_truthTable :: ([Expression],[[Bool]])
_truthTable = getTruthTable _exprObj

{-
Test:
parseExpr "(P>Q)|(R&S)" == Disjunction (Implication LiteralP LiteralQ) (Conjunction LiteralR LiteralS)
_truthTable == ([LiteralP,LiteralQ,LiteralR,LiteralS,Implication LiteralP LiteralQ,Conjunction LiteralR LiteralS,Disjunction (Implication LiteralP LiteralQ) (Conjunction LiteralR LiteralS)],[[True,True,True,True,True,True,True],[True,True,True,False,True,False,True],[True,True,False,True,True,False,True],[True,True,False,False,True,False,True],[True,False,True,True,False,True,True],[True,False,True,False,False,False,False],[True,False,False,True,False,False,False],[True,False,False,False,False,False,False],[False,True,True,True,True,True,True],[False,True,True,False,True,False,True],[False,True,False,True,True,False,True],[False,True,False,False,True,False,True],[False,False,True,True,True,True,True],[False,False,True,False,True,False,True],[False,False,False,True,True,False,True],[False,False,False,False,True,False,True]])
-}

