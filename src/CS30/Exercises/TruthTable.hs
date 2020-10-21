{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.TruthTable where

import CS30.Data
import CS30.Exercises.Data -- 'exerciseType' function

import Data.Void
import Data.Functor.Identity
import Data.List (sort)
import Data.Aeson.TH -- for deriveJSON
import qualified Data.Map as Map
import qualified Data.Text as Text

import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

import Control.Monad.Combinators.Expr
import Control.Monad (void)

import Debug.Trace

--------------------------------------Data and Types-------------------------------------------

type Parser = ParsecT Void String Identity -- parsing strings in this file
type ParseError = ParseErrorBundle String Void -- corresponding error type

data Expression
  = LiteralP | LiteralQ | LiteralR | LiteralS 
  | Conjunction Expression Expression
  | Disjunction Expression Expression
  | Implication Expression Expression
  | Negation Expression
  deriving (Show, Eq, Ord)

type Literal = Expression
type TruthTable = ([Expression], [[Bool]])

---------------------------------Character Definitions-------------------------------------------

-- Literals
charP, charQ, charR, charS :: Char
charP = 'P'
charQ = 'Q'
charR = 'R'
charS = 'S'

-- Operations
charNeg, charCon, charDis, charImp :: Char
charNeg = '~'
charCon = '&' -- 'Λ'
charDis = '|' -- '∨'
charImp = '>' -- '→'

-- Booleans
charTrue, charFalse :: Char
charTrue = 'T'
charFalse = 'F'

-- Parenthesis
charOpen, charClose :: Char
charOpen = '('
charClose = ')'

--------------------------------------Parser Functions-------------------------------------------

-- THE MOST IMPORTANT ONE :)
-- Converts string to an expression
parseExpr :: String -> Expression
parseExpr = getExpr . parseExpr'

parseExpr' :: String -> Either ParseError Expression -- Helper
parseExpr' x = parse parserExpression "<myparse>" (filter (\c->c/=' ') x)

getExpr :: Either ParseError Expression -> Expression -- TODO : Make exhaustive
getExpr (Right x) = x

-- Converts an expression object to a string
showExpr :: Expression -> String
showExpr LiteralP = [charP]
showExpr LiteralQ = [charQ]
showExpr LiteralR = [charR]
showExpr LiteralS = [charS]
showExpr (Negation e) = charNeg : showExpr e
showExpr (Conjunction e1 e2) = [charOpen] ++ showExpr e1 ++ " " ++ [charCon] ++ " " ++ showExpr e2 ++ [charClose]
showExpr (Disjunction e1 e2) = [charOpen] ++ showExpr e1 ++ " " ++ [charDis] ++ " " ++ showExpr e2 ++ [charClose]
showExpr (Implication e1 e2) = [charOpen] ++ showExpr e1 ++ " " ++ [charImp] ++ " " ++ showExpr e2 ++ [charClose]

-- Instantiates literals and evaluates the boolean value of an expression
evalExpr :: Expression -> Map.Map Literal Bool -> Bool
evalExpr (Negation e) literalMapping = not (evalExpr e literalMapping)
evalExpr (Conjunction e1 e2) literalMapping = evalExpr e1 literalMapping && evalExpr e2 literalMapping
evalExpr (Disjunction e1 e2) literalMapping = evalExpr e1 literalMapping || evalExpr e2 literalMapping
evalExpr (Implication e1 e2) literalMapping = not (evalExpr e1 literalMapping && not (evalExpr e2 literalMapping))
evalExpr literal literalMapping = if Map.member literal literalMapping then literalMapping Map.! literal else True

-------------------------------------Parser Dependencies----------------------------------------

parserExpression :: Parser Expression
parserExpression = makeExprParser parserTerm operatorTable

parserTerm :: Parser Expression
parserTerm = LiteralP <$ char charP
   <|> LiteralQ <$ char charQ
   <|> LiteralR <$ char charR
   <|> LiteralS <$ char charS
   <|> parserParenthesis parserExpression

parserParenthesis :: Parser a -> Parser a
parserParenthesis = between (symbol [charOpen]) (symbol [charClose])

-------------------------------------Parser Utilities----------------------------------------

symbol :: String -> Parser String
symbol = L.symbol space

binary :: String -> (Expression -> Expression -> Expression) -> Operator Parser Expression
binary  name f = InfixL  (f <$ symbol name)

prefix :: String -> (Expression -> Expression) -> Operator Parser Expression
prefix  name f = Prefix  (f <$ symbol name)

-------------------------------------Parser Operation Table----------------------------------

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ 
    [prefix [charNeg] Negation],
    [binary [charCon] Conjunction],
    [binary [charDis] Disjunction],
    [binary [charImp] Implication]
  ]

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

--------------------------------------Truth Table Conversion--------------------------------------

fromTruthTable :: TruthTable -> Field
fromTruthTable (ttHeader, ttBody) = FTable (ftHeader : ftBody)
                                    where ftHeader = map exprToHeader ttHeader
                                          ftBody = map (\ttBodyRow -> map boolToCell ttBodyRow) ttBody

exprToHeader :: Expression -> Cell
exprToHeader e = Header (FText (showExpr e))

boolToCell :: Bool -> Cell
boolToCell e = Cell (FText (if e then [charTrue] else [charFalse]))

toTruthTable :: Field -> TruthTable
toTruthTable (FTable ft) = (ttHeader, ttBody)
                           where ttHeader = map headerToExpr (head ft)
                                 ttBody = map (\ftBodyRow -> map cellToBool ftBodyRow) (take 1 ft)

headerToExpr :: Cell -> Expression
headerToExpr (Header (FText x)) = parseExpr x

cellToBool :: Cell -> Bool
cellToBool (Cell (FText x)) = x == [charTrue]

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

-- Checks whether two expressions are semantically equal (Each corresponding node in parse has same unordered set of children)
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
