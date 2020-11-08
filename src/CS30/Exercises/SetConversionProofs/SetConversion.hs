{-# LANGUAGE TemplateHaskell #-}

{-
authors: Donia Tung
COSC 69.14, 20F
Group Assignment 2
-}

module CS30.Exercises.SetConversionProofs.SetConversion (setConv) where
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
import CS30.Exercises.SetConversionProofs.SetExprParser

-- setConv definition for export to Pages.hs
setConv :: ExerciseType
setConv = exerciseType "Set Conversion" "L??" "Conversion to and from set-builder notation" 
            setConversion
            setConvQuer 
            setConvFeedback


-- data SetExpr = Var String -- single variable
--               | SetBuilder SetExpr -- set builder expression
--               | Power SetExpr   -- powerset
--               | Cap SetExpr SetExpr     -- cap operation, intersection
--               | Cup SetExpr SetExpr     -- cup operation, union
--               | SetMinus  SetExpr SetExpr  -- set difference
--               | Wedge SetExpr SetExpr   -- and, intersection
--               | Vee SetExpr SetExpr     -- or, union
--               | In SetExpr      -- element of 
--               | NotIn SetExpr   -- not an element of
--               | Subset SetExpr -- subset of
--               deriving Show

-- fxn for generating a random expression, using \cap, \cup, and \setminus operators (as specified in the assignment sheet)
generateRandEx :: Int -> ChoiceTree SetExpr
generateRandEx i | i < 1
 = Branch [ Branch [Node (Var varName) | varName <- ["A","B","C"]]
          ]
generateRandEx i
 = Branch [do {e1 <- generateRandEx i'
              ;e2 <- generateRandEx (i - i' - 1)
              ;opr <- nodes [Cap, Cup, SetMinus]
              ;return (opr e1 e2)
              }
          | i' <- [0..i-1]
          ]

setConversion :: [ChoiceTree ([Field], [Integer])]
setConversion = [ 
            -- cardinality of the cartesian product of two sets
           nodes [ ( [FText"the question"] -- rule                     
                     , [n1 * n2, n1, n2] -- calculated solution (head of list) and all # allowed in answer (tail of list)
                     )
                   | n1 <- [1..10], n2 <- [1..10]]
            -- cardinality of a power set
        --     , nodes [ ( [[FMath"|A| ", FMath$ "= "++(show n1)], 
        --                 [FText "|𝒫",FMath$ "(A)", FText"|"],
        --                 [FMath$ "2^{"++(show n1)++"}" ]
        --                 ]
        --              , [2^n1, 2, n1]
        --              )
        --            | n1 <- allCards]
        --     -- cardinality of set x its powerset
        --    , Branch [ nodes [ ( [[FMath"|A| ", FMath$"= "++(show n1)],
        --                         [FMath$"|A \\times ", FText"𝒫", FMath"(A)|"],
        --                         [FMath$ (show n1)++"\\cdot 2^{"++(show n1)++"}"]
        --                         ]
        --                       ,[n1*(2^n1), 2, n1]
        --                       )
        --                     | n1 <- allCards]
        --              ] 
        --     -- cardinality with set builder notatino (like ex from the assignment sheet)
        --    , Branch [ nodes [ ( [[FMath"|B| ", FMath$ "= "++(show n1)],
        --                          [FMath$"\\left|\\left\\{A\\ \\mid\\ A \\subseteq B, \\ |A| ="++(show n2)++"\\right\\}\\right|"], 
        --                          [FMath$"\\binom{"++(show n1)++"}{"++(show n2)++"}"] -- e.g. \binom{10}{6}
        --                         ]
        --                       , [n1 `choose` n2, n1, n2, n1-n2]
        --                       )
        --                     | n1 <- allCards, n2 <- [1..n1-1] ]
        --             ]
           ]

-- for generating the actual text of the question displayed to client
setConvQuer :: ([Field],[Integer]) -> Exercise -> Exercise
setConvQuer (quer, _solution) exer 
  = exer { eQuestion=[FText "This is thequestion:  "] ++ quer ++
                     [FFieldMath "answer"]}
  

-- fxn for generating feedback to the users, based on their parsed input
setConvFeedback :: ([Field],[Integer]) -> Map.Map String String -> ProblemResponse -> ProblemResponse
setConvFeedback (quer, sol) mStrs defaultRsp 
  = reTime $ case Map.lookup "answer" mStrs of
      Just v -> markCorrect $ 
                defaultRsp {prFeedback = [FText("you entered" ++ show v)]}
      Nothing -> markWrong $
                defaultRsp {prFeedback = [FText("wrong, you entered nothing")]}


----------- SHOWING SETEXPR AS STRINGS ------
prec :: SetExpr -> Int 
prec (Cup _ _) = 2
prec (Cap _ _) = 2
prec (SetMinus _ _) = 2
prec (Power _) = 2

symb :: SetExpr -> String 
symb (Var a) = a
symb (Cup e1 e2) = myShow e1 ++ "\\cup" ++ myShow e2
symb (Cap e1 e2) = myShow e1 ++ "\\cap" ++ myShow e2
symb (SetMinus e1 e2) = myShow e1 ++  "\\setminus" ++ myShow e2
symb (Power e) = "\\P(" ++ myShow e ++ ")"

showSpace :: String
showSpace = " "

myShow :: SetExpr -> String
myShow (Var n) = show n 
myShow (Cap e1 e2) 
  = "(for all " ++ vars ++ showsPrec' p ((Cap e1 e2))
    where p = prec (Cap e1 e2)
          vars = list_to_string ( getVars (Cap e1 e2) )
   
showParen' :: Bool -> String -> String
showParen' p x = if p then
                "(" ++ x ++  ")"
                else x

showsPrec' :: Int -> SetExpr -> String
showsPrec' p (Var n) = show n
-- showsPrec' p (op (x:xs))
--     = showParen' (p<q) (showsPrec' q x ++ showSpace ++
--             symb op ++ showSpace ++ showsPrec' (q-1) (head xs))
--       where q = prec op

-- fxn for extracting the variables in a set expression (not in use rn)
getVars :: SetExpr -> [String]
getVars (Var x) = [x]
getVars (Cap e1 e2) = getVars e1  ++ getVars e2
getVars (Cup e1 e2) = getVars e1 ++ getVars e2
getVars (SetMinus e1 e2) = getVars e1 ++ getVars e2
getVars (Wedge e1 e2) = getVars e1 ++ getVars e2
getVars (Vee e1 e2) = getVars e1 ++ getVars e2
getVars (Power e) = getVars e
getVars (In e) = getVars e
getVars (NotIn e) = getVars e
getVars (SetBuilder e) = getVars e
getVars (Subset e) = getVars e

-- helper fxn to get from list to strings
list_to_string :: [String] -> String
list_to_string = intercalate ", " . map show

