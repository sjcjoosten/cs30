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
import CS30.Exercises.SetConversionProofs.SetExprParser (parseExpr, parseUntil, symbol, SetExpr)

type Parser = ParsecT Void String Identity
data Law = Law String (Equation) 
            deriving Show
type Equation = (SetExpr, SetExpr)

-- 5 given laws from assignment sheet 
law1, law2, law3, law4, law5 :: String 
law1 = "cap definition:A \\cap B = \\left\\{e| e \\in A \\wedge e\\in B\\right\\}"
law2 = "cup definition:A \\cup B = \\left\\{e| e \\in A \\vee e\\in B\\right\\}"
law3 = "set minus definition:A \\setminus B = \\left\\{e| e \\in A \\wedge e\\notin B\\right\\}"
law4 = "powerset definition:\\P(A) = \\left\\{e| e \\subseteq A\\right\\}"
law5 = "identity fxn:e\\in \\left\\{e|p\\right\\} = p"

laws :: [String] 
laws = [law1, law2, law3, law4, law5]

--fxn for parsing laws 
parseLaw :: Parser Law
parseLaw 
    = do lawName <- parseUntil ':'
         lhs <- parseExpr
         _ <- symbol "="
         rhs <- parseExpr
         return (Law lawName (lhs, rhs))

-- maps the law parser onto the list of laws
parsedLaws :: [Law]
parsedLaws = map strToLaw laws

-- helper function for mapping, returns a successfully parsed law
strToLaw :: String -> Law 
strToLaw law
   =  case parse parseLaw "" law of
                Right st -> st
                Left _ -> error "problem in parsing the law"
