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
import CS30.Exercises.SetConversionProofs.SetExprParser (parseExpr, parseUntil, symbol,  SetExpr)


type Parser = ParsecT Void String Identity
data Law = Law String (Equation) 
            deriving Show
type Equation = (SetExpr, SetExpr)

law1, law2, law3, law4, law5 :: String 
law1 = "law name:A \\cap B = \\left\\{e| e\\in A \\wedge e\\in B\\right\\}"
law2 = "law name2:A \\cup B = \\left\\{e| e\\in A \\vee e\\in B\\right\\}"
law3 = "law name3:A \\setminus B = \\left\\{e| e\\in A \\wedge e\\notin B\\right\\}"
law4 = "law name4:\\P(A) = \\left\\{e| e\\subseteq A\\right\\}"
law5 = "law name5:e\\in \\left\\{e|p\\right\\} = p"

laws :: [String] 
laws = [law1, law2, law3, law4, law5]

-- name: x + y = y + x
parseLaw :: Parser Law
parseLaw 
    = do lawName <- parseUntil ':'
         lhs <- parseExpr
         _ <- symbol "="
         rhs <- parseExpr
         return (Law lawName (lhs, rhs))

