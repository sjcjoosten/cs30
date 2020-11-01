{-# LANGUAGE TemplateHaskell #-}

{-
authors: Donia Tung
COSC 69.14, 20F
Group Assignment 2
-}

module CS30.Exercises.SetConversionProofs.LawParser  where

import           Data.Functor.Identity
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char -- readFile
import CS30.Exercises.SetConversionProofs.SetExprParser (parseExpr, parseUntil, exprParens, SetExpr)


type Parser = ParsecT Void String Identity
data Law = Law String (Equation)
type Equation = (SetExpr, SetExpr)

-- name: x + y = y + x
parseLaw :: Parser Law
parseLaw 
    = do lawName <- parseUntil ':'
         _ <- string ":"
         lhs <- parseLeft
         _ <- string "="
         rhs <- parseExpr
         return (Law lawName (lhs, rhs))

parseLeft :: Parser SetExpr
parseLeft
    = do _ <- exprParens parseExpr
         rule <- parseExpr 
         return rule

