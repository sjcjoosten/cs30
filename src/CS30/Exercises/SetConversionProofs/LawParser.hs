{-# LANGUAGE TemplateHaskell #-}

{-
authors: Donia Tung & Mikio Obuchi 
COSC 69.14, 20F
Group Assignment 2
-}
module CS30.Exercises.SetConversionProofs.LawParser  where
import           Data.Functor.Identity
import           Data.Void
import           Text.Megaparsec
import           Data.List 
import CS30.Exercises.SetConversionProofs.SetExprParser (parseExpr, parseUntil, symbol, SetExpr(..))

type Parser = ParsecT Void String Identity
data Law = Law String (Equation) 
            deriving Show
type Equation = (SetExpr, SetExpr)


law1, law2, law3, law4, law5, law6, law7, law8, law9, law10, law11, law12, law13 :: String 
-- 5 given laws from assignment sheet 
law1 = "Intersection Definition:A \\cap B = \\left\\{e| e \\in A \\wedge e\\in B\\right\\}"
law2 = "Union Definition:A \\cup B = \\left\\{e| e \\in A \\vee e\\in B\\right\\}"
law3 = "Set Difference Definition:A \\setminus B = \\left\\{e| e \\in A \\wedge e\\notin B\\right\\}"
law4 = "Powerset Definition:\\P(A) = \\left\\{e| e \\subseteq A\\right\\}"
law5 = "Identity Function:e\\in \\left\\{e|p\\right\\} = p"
-- creative element: additional laws, used in subsequent levels 
law6 = "Union Associativity:(A \\cup B) \\cup C = A \\cup (B \\cup C)"
law7 = "Intersection Associativity:(A \\cap B) \\cap C = A \\cap (B \\cap C)"
law8 = "Union Idempotent Law:A \\cup A = A"
law9 = "Intersection Idempotent Law:A \\cap A = A"
law10 = "Union Distributivity:A \\cup (B \\cap C) = (A \\cup B) \\cap (A \\cup C)"
law11 = "DeMorgans Law:A \\setminus (B \\cup C) = (A \\setminus B) \\cap (A \\setminus C)"
law12 = "DeMorgans Law:A \\setminus (B \\cap C) = (A \\setminus B) \\cup (A \\setminus C)"
law13 = "Name:(A \\setminus B) \\cup B = A \\cup B"

basicLaws, advancedLaws :: [String] 
basicLaws = [law1, law2, law3, law4, law5]
advancedLaws = [law6, law7, law8, law9, law10, law13, law1, law2, law3, law4, law5]

--fxn for parsing laws 
parseLaw :: Parser Law
parseLaw 
    = do lawName <- parseUntil ':'
         lhs <- parseExpr
         _ <- symbol "="
         rhs <- parseExpr
         return (Law lawName (lhs, rhs))

-- maps the law parser onto the list of laws, using basic 5 laws from assignment sheet
-- runs laws through validateLaw function to make sure their valid before adding them to returned list
parsedBasicLaws :: [Law]
parsedBasicLaws = filter (\x -> validateLaw x == True) (map strToLaw basicLaws)

-- maps the law parser onto the list of laws, using additional laws
-- runs laws through validateLaw function to make sure their valid before adding them to returned list
parsedAdvancedLaws :: [Law]
parsedAdvancedLaws= filter (\x -> validateLaw x == True) (map strToLaw advancedLaws)

-- helper function for mapping, returns a successfully parsed law
strToLaw :: String -> Law 
strToLaw law
   =  case parse parseLaw "" law of
                Right st -> st
                Left _ -> error "problem in parsing the law"

validateLaw :: Law -> Bool
validateLaw (Law _nm (e1, e2)) = sort (evaluate e1) == sort (evaluate e2)

-- evaluate fxn, calcualtes values for expressions to test that they are valid 
-- generates [int] representation for a set, need to compare two to evaluate, only does #, not sets of numbers
evaluate :: SetExpr -> [Int]
evaluate (Var "A") = [0,1,2,3]
evaluate (Var "B") = [0,2,4,6]
evaluate (Var "C") = [0,1,4,5]
evaluate (Var "D") = [2,3,5,6] 
evaluate (Var _v) = [] -- should never reach this case if var assignment happens correctly 
evaluate (Cup e1 e2) = (evaluate e1) `union` (evaluate e2)
evaluate (Vee e1 e2) = (evaluate e1) `union` (evaluate e2)
evaluate (Cap e1 e2) = (evaluate e1) `intersect` (evaluate e2)
evaluate (Wedge e1 e2) = (evaluate e1) `intersect` (evaluate e2)
evaluate (SetMinus e1 e2) = (evaluate e1) \\ (evaluate e2)
evaluate (NotIn e) = [0,1,2,3,4,5,6] \\ (evaluate e)
evaluate (In e) = evaluate e
evaluate (SetBuilder e) = evaluate e
evaluate (Power _e) = [] --powerset (evaluate e)
evaluate (Subset _e) = []--powerset (evaluate e)

-- fxn for building out the powerset of a variable list
-- commented because not used, but was an alternative for defining evaluate on a power/subset eq
-- powerset :: [a] -> [[a]]
-- powerset [] = [[]]
-- powerset (x:xs) = [x:ps | ps <- powerset xs] ++ powerset xs

