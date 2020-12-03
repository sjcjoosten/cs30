{-# OPTIONS_GHC -Wall #-}
module CS30.Exercises.TruthTable (truthEx) where

import CS30.Data
import CS30.Exercises.Data -- 'exerciseType' function

import Data.Void
import Data.Functor.Identity
import Data.List (sort)
import Data.Maybe

import qualified Data.Map as Map

import Text.Megaparsec hiding (ParseError)
import Text.Megaparsec.Char

import Control.Monad.Combinators.Expr


-------------------------------------- Exercise Generation ------------------------------------

truthEx :: ExerciseType
truthEx = exerciseType "TruthTable" "Anmol and Sanket"
          "Logic: Complete a TruthTable"
          trtable -- List of problems
          genQuestion -- present question as 'Exercise'
          genFeedback -- handle response as 'ProblemResponse'

trtable :: [ChoiceTree (Field, [String])]
trtable = [ nodes (generateProb (fromTruthTable (getTruthTable (parseExpr "(~P&Q)"))) 2)
          , nodes (generateProb (fromTruthTable (getTruthTable (parseExpr "(~P&Q) | (~P|Q)"))) 3)
          -- too slow: , nodes (generateProb (fromTruthTable (getTruthTable (parseExpr "(~P&Q) | (~P|Q) & (~R|Q) > (S&Q)"))) 4)
          ]

genQuestion :: (Field, [String]) -> Exercise -> Exercise
genQuestion (quer, _solution) ex = ex{eQuestion=[ FText $"Fill the missing values in the following table. "] ++ [quer]}

genFeedback :: (Field, [String]) -> Map.Map String String -> ProblemResponse -> ProblemResponse
genFeedback (_, sol) mStrs resp = case (catMaybes (compareSol mStrs sol)) of
  [] -> markCorrect $ resp{prFeedback=[FText "Solution is correct!"]}
  -- then error "Solution is incorrect! Better Luck Next Time :("
  lst -> markWrong $ resp{prFeedback=[FText$ "You have "++show (length lst)++" incorrect responses"]} 

------------------------------------- Exercise Generation Helpers ---------------------------------

generateProb :: Field -> Int -> [(Field, [String])]
generateProb (FTable xs) nblanks = [(getBlankTable (FTable xs) blanks 0, getBlankSol (FTable xs) blanks)| blanks <- orderedSubsets nblanks cells]
  where
    cells = [0..(length (concat xs)-1)]
generateProb _ _ = error "expecting FTable"

-- Takes previously filled Truth Table and inserts empty cells at given indices
getBlankTable :: Field -> [Int] -> Int -> Field
getBlankTable (FTable xs) [] _ = FTable xs
getBlankTable (FTable xs) (b:bs) enum = getBlankTable (FTable [[insertCell (snd ri) cj | cj <- zip (fst ri) cindices]| ri <- zip xs rindices]) bs (enum+1)
  where
    nrows = length xs
    ncols = length (xs !! 0)
    rindices = [0..(nrows-1)]
    cindices = [0..(ncols-1)]
    brow = b `div` ncols
    bcol = b `mod` ncols
    insertCell i (Header (FText x), j) = if i == brow && j == bcol then (Header (FFieldMath (show enum))) else (Header (FText x))
    insertCell i (Cell (FText x), j) = if i == brow && j == bcol then (Cell (FFieldMath (show enum))) else (Cell (FText x))
    insertCell _i (c, _) = c
getBlankTable _ _ _ = error "unexpected non-table"

-- Takes prev filled Truth Table and extracts the solutions from cells at given indices
getBlankSol :: Field -> [Int] -> [String]
getBlankSol (FTable xs) blanks = [sol b | b<-blanks]
  where
    ncols = length (xs !! 0)
    sol b = getFTextStr ((xs !! (b `div` ncols)) !! (b `mod` ncols))
    getFTextStr (Cell (FText x)) = x
    getFTextStr (Header (FText x)) = x
    getFTextStr _ = error "unexpected non-table data"
getBlankSol _ _ = error "unexpected non-table field"

-- Returns all combinations of size n from list m
orderedSubsets :: Int -> [Int] -> [[Int]]
orderedSubsets 0 _xs = [[]]
orderedSubsets n xs@(h:t)
  | length xs < n = []
  | otherwise   = orderedSubsets n t ++ map (h:) (orderedSubsets (n - 1) t)
orderedSubsets _ [] = []

compareSol :: Map.Map String String -> [String] -> [Maybe String]
compareSol mStrs sol = [case Map.lookup k mStrs of 
                        Nothing -> error "server expected other input"
                        Just a -> if a == v then Nothing else Just a | (k, v) <- zip (map show [0..((length sol)-1)]) sol]

--------------------------------------Data and Types-------------------------------------------

type Parser = ParsecT Void String Identity -- parsing strings in this file
type ParseError = ParseErrorBundle String Void -- corresponding error type

data Expression
  = LiteralP | LiteralQ | LiteralR | LiteralS 
  | Conjunction Expression Expression
  | Disjunction Expression Expression
  | Implication Expression Expression
  | Negation Expression
  | ExpressionError
  deriving (Show, Eq, Ord)

type Literal = Expression
data TruthTable = TruthTable [Expression] [[Bool]]

---------------------------------Character Definitions-------------------------------------------

-- Literals
charP, charQ, charR, charS :: Char
charP = 'P'
charQ = 'Q'
charR = 'R'
charS = 'S'

-- Operations -- TODO : How to make special characters work?
charNeg, charCon, charDis, charImp :: Char
charNeg = '~'
charCon = '&' -- '\923' -- 'Λ' -- 
charDis = '|' -- '\8744' -- '∨' -- 
charImp = '>' -- '\8594' -- '→' -- 

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
parseExpr' x = parse parserExpression "" (filter (\c->c/=' ') x)

getExpr :: Either ParseError Expression -> Expression
getExpr (Right x) = x
getExpr (Left _x) = ExpressionError

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
showExpr ExpressionError = "Parsing error"

-- Instantiates literals and evaluates the boolean value of an expression
evalExpr :: Expression -> Map.Map Literal Bool -> Bool
evalExpr (Negation e) literalMapping = not (evalExpr e literalMapping)
evalExpr (Conjunction e1 e2) literalMapping = evalExpr e1 literalMapping && evalExpr e2 literalMapping
evalExpr (Disjunction e1 e2) literalMapping = evalExpr e1 literalMapping || evalExpr e2 literalMapping
evalExpr (Implication e1 e2) literalMapping = not (evalExpr e1 literalMapping && not (evalExpr e2 literalMapping))
evalExpr literal literalMapping = if Map.member literal literalMapping then literalMapping Map.! literal else True

-------------------------------------Megaparsec stuff----------------------------------------

parserExpression :: Parser Expression
parserExpression = makeExprParser parserTerm operatorTable

parserTerm :: Parser Expression
parserTerm = LiteralP <$ char charP <|>
             LiteralQ <$ char charQ <|>
             LiteralR <$ char charR <|>
             LiteralS <$ char charS <|>
             parserParenthesis parserExpression

parserParenthesis :: Parser a -> Parser a
parserParenthesis = between (string [charOpen]) (string [charClose])

operatorTable :: [[Operator Parser Expression]]
operatorTable =
  [ 
    [Prefix (Negation <$ string [charNeg])],
    [InfixL (Conjunction <$ string [charCon])],
    [InfixL (Disjunction <$ string [charDis])],
    [InfixL (Implication <$ string [charImp])]
  ]

--------------------------------------Truth Table Logic-------------------------------------------

-- This returns the truth table for an expression (Header: [Expression], Body: [[Bool]])
getTruthTable :: Expression -> TruthTable
getTruthTable e = TruthTable ttHeader ttBody
                  where ttHeader = flattenExpr e ++ [e]
                        ttBody = getTruthTableBody ttHeader (getAllLiteralMappings e)

-- This returns the truth table body given a header and a literal to boolean mapping for every row
getTruthTableBody :: [Expression] -> [Map.Map Literal Bool] -> [[Bool]]
getTruthTableBody header literalMappings = 
  map (\literalMapping -> map (\headerExp -> evalExpr headerExp literalMapping 
                              ) header) literalMappings

-- Converts truth table to field FTable
fromTruthTable :: TruthTable -> Field
fromTruthTable (TruthTable ttHeader ttBody) 
      = FTable (ftHeader : ftBody)
        where exprToHeader e = Header (FText (showExpr e))
              boolToCell e = Cell (FText (if e then [charTrue] else [charFalse]))
              ftHeader = map exprToHeader ttHeader
              ftBody = map (\ttBodyRow -> map boolToCell ttBodyRow) ttBody


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

-- Whether an expression is a pure literal
isLiteral :: Expression -> Bool
isLiteral x = elem x [LiteralP, LiteralQ, LiteralR, LiteralS]