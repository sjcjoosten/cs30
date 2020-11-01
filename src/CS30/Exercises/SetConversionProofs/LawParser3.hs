{-# LANGUAGE TemplateHaskell #-}

{-
authors: Bennett Clark & Donia Tung
COSC 69.14, 20F
Group Assignment 1 
-}

module CS30.Exercises.SetConversionProofs.LawParser3 (proofEx) where
import           CS30.Data
import           CS30.Exercises.Data
import CS30.Exercises.Util ( reTime )
import           Data.List.Extra (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Functor.Identity
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char -- readFile
import qualified Text.Megaparsec.Char.Lexer as L
import           Control.Monad.Combinators.Expr
import Debug.Trace


allCards :: [Integer]
allCards = [1..99] 

proofEx :: ExerciseType
proofEx = exerciseType "Cardinality" "L??" "Cardinality of Expression" 
            cardinality
            cardQuer 
            cardFeedback

cardinality :: [ChoiceTree ([[Field]], [Integer])]
cardinality = [ 
            -- cardinality of the cartesian product of two sets
           nodes [ ( [[FMath$ "|A| = "++(show n1), FText" and ", FMath$"|B| = "++(show n2)], -- rule
                      [FMath$ "|A \\times B|"],  -- question 
                      [FMath$ (show n1), FMath$"\\cdot",  FMath$ (show n2)] -- intermediate step, before # solution
                     ]
                     , [n1 * n2, n1, n2] -- calculated solution (head of list) and all # allowed in answer (tail of list)
                     )
                   | n1 <- allCards, n2 <- allCards]
            -- cardinality of a power set
            , nodes [ ( [[FMath"|A| ", FMath$ "= "++(show n1)], 
                        [FText "|𝒫",FMath$ "(A)", FText"|"],
                        [FMath$ "2^{"++(show n1)++"}" ]
                        ]
                     , [2^n1, 2, n1]
                     )
                   | n1 <- allCards]
           ]

-- for generating the actual text of the question displayed to client
cardQuer :: ([[Field]],[Integer]) -> Exercise -> Exercise
cardQuer (quer, _solution) exer 
  = exer { eQuestion=[FText "Given "] ++ rule ++ 
                     [FText ", compute "] ++ question ++ 
                     [FFieldMath "answer"]}
                        where rule = head quer
                              question = quer!!1

-- helper fxn to get from list to strings
list_to_string :: [Integer] -> String
list_to_string = intercalate ", " . map show

-- fxn for generating feedback to the users, based on their parsed input
cardFeedback :: ([[Field]],[Integer]) -> Map.Map String String -> ProblemResponse -> ProblemResponse
cardFeedback (quer, sol) mStrs defaultRsp 
  =   trace("direct input " ++ show mStrs)
      reTime $ case pr of 
      Just v ->  -- trace ("this is what's stored as input: " ++ show pr)
                markCorrect $ defaultRsp{prFeedback = [FText"Correct! "] ++ question ++ [FMath " = "] ++ step ++ [FMath " = "] ++ [FMath (show $ head sol)]} 
      Nothing -> markWrong $ defaultRsp{prFeedback = [FText("The correct answer is ")] ++ question ++ [FMath " = "] ++ step ++ [FMath " = "] ++ [FMath (show $ head sol)] ++ [FText". We couldn't understand your answer."]}
    where usr = Map.lookup "answer" mStrs
          question = quer!!1
          step = quer!!2
          -- parse the user input
          pr :: Maybe SetExpr
          pr = case usr of
                 Nothing ->  trace ("nothing from case")
                             Nothing
                 Just v -> case parse (parseExpr <* eof) "" v of
                             Left _ -> trace ("failure to parse")
                                       Nothing 
                             Right st -> trace ("parsed input: " ++ show st)
                                         Just st


-- datatype that we try to parse all lawsinto 
data SetExpr = Var String
              | Power SetExpr   -- set first SetExpr to the second SetExpr power
              | Cap SetExpr SetExpr           -- factorial
              | Cup SetExpr SetExpr  -- division
              | SetMinus  SetExpr SetExpr  -- multiply two SetExpr's 
              | Wedge SetExpr SetExpr -- first SetExpr `choose` second SetExpr
              | Vee SetExpr SetExpr
              | In SetExpr
              | NotIn SetExpr
              | Subset SetExpr
              deriving Show

-- \left\{e | e \in A \wedge e\notin B\right\} 
-- --> Wedge (In (Var A)) (NotIn (Var B))
-- VS 
-- In (Wedge (Var "A") (NotIn (Var "B")))

-- \left\{e | e \subseteq A\right\}
-- --> Subset(A)

-- fxn for extracting the variables in a set expression 
getVars :: SetExpr -> String
getVars (Var x)    = x
getVars (Cap e1 e2)     = getVars e1  ++ getVars e2
getVars (Cup e1 e2) = getVars e1++getVars e2
getVars (SetMinus e1 e2) = getVars e1++getVars e2
getVars (Power e1) = getVars e1
getVars (Wedge e1 e2) = getVars e1++getVars e2

type Parser = ParsecT Void String Identity

-- parse spaces (used w/ symbol and lexeme)
spaceConsumer :: Parser ()
spaceConsumer = L.space spaces empty empty 

-- based on Drill 6.2 scaffold
spaces :: Parser ()
spaces = some spc >> return ()
 where spc = string " " <|> string "\t" <|> string "\n" <|> string "\r"
             <|> string "\\ " <|> string "~"

-- parse a given specific string, accounting for spaces 
symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

-- parse some lexeme, accounting for spaces
lexeme :: Parser a -> Parser a
lexeme   = L.lexeme spaceConsumer

-- parse something between {...}
brackets :: Parser a -> Parser a
brackets = between (symbol "\\left\\{") (symbol "\\right\\}")

-- parse something between (...) 
parens :: Parser a -> Parser a          
parens = between (symbol "\\left(") (symbol "\\right)")

-- parse something between (...) 
exprParens :: Parser a -> Parser a          
exprParens = between (symbol "(") (symbol ")")

-- parse something of the form e | e \subseteq A
suchThat :: Parser a -> Parser a 
suchThat = between (symbol "e|e") (symbol "")

withE :: Parser a -> Parser a 
withE = between (symbol "e") (symbol "")

-- parses some variable alone into SetExpr
parseConstant :: Parser SetExpr
parseConstant = do
  n <- lexeme L.charLiteral
  return (Var [n])

-- operator table for use with makeExprParser 
operatorTable :: [[Operator Parser SetExpr]]
operatorTable =
  [ [binary "\\cap" Cap], 
    [binary "\\cup" Cup], 
    [binary "\\setminus" SetMinus],
    [binary "\\wedge" Wedge],  
    [binary "\\vee" Vee], 
    [prefix "\\mathbb{P}" Power], 
    [prefix "\\P" Power], 
    [prefix "\\in" In], 
    [prefix "\\notin" NotIn], 
    [prefix "\\subseteq" Subset]
  ]

-- helper function for generating an binary infix operator
-- based on documentation for Control.Monad.Combinators.Expr
binary :: String -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ symbol name)

-- helper function for generating a prefix operator
-- based on documentation for Control.Monad.Combinators.Expr
prefix :: String -> (a -> a) -> Operator (ParsecT Void String Identity) a
prefix  name f = Prefix (f <$ symbol name)

parseUntil :: Char -> Parser String -- something wrong here
parseUntil c
  = (do _ <- satisfy (== c)
        return []) <|> 
    (do accum <- satisfy (const True) 
        rmd <- parseUntil c 
        return (accum:rmd) 
        )

-- parse a fraction, like \frac{4}{2}
parseSetBuilder :: Parser SetExpr
parseSetBuilder 
  = do
    _ <- parseUntil '|'
    _ <- string "|"
    remander <-  parseExpr
    return remander


-- parses a term (some expression in brackets/parens, a constant alone, or an expression in set builder notation)
parseTerm :: Parser SetExpr
parseTerm = parens parseExpr <|> exprParens parseExpr <|> brackets parseExpr <|> suchThat parseExpr  <|> withE parseExpr <|> parseConstant -- <|> parseSetBuilder

-- parse a set expression (using makeExprParser)
parseExpr :: Parser SetExpr
parseExpr =  makeExprParser parseTerm operatorTable
