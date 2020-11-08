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
import CS30.Exercises.SetConversionProofs.SetExprParser


-- setConv definition for export to Pages.hs
setConv :: ExerciseType
-- setConv = exerciseType "Set Conversion" "L??" "Conversion to set-builder notation" 
--             setConversion
--             setConvQuer 
--             setConvFeedback
setConv = exerciseType "Set Conversion" "L??" "Conversion to set-builder notation" 
              [permutations 5] genProof simpleFeedback
        where simpleFeedback sol rsp pr
               = reTime $ case Map.lookup "proof" rsp of
                   Just str
                     -> if map ((sol !!) . read) (breakUnderscore str) == [0..4]
                        then markCorrect pr
                        else markWrong pr{prFeedback=[FText "You answered: ",FText str]}
                   Nothing -> error "Client response is missing 'proof' field"

permutations :: Int -> ChoiceTree [Int]
permutations 0 = Node []
permutations n -- ChoiceTree is a Monad now! I've also derived "Show", so you can more easily check this out in GHCI.
 = do i <- nodes [0..n-1]
      rm <- permutations (n-1)
      return (i : map (\v -> if v >= i then v+1 else v) rm)

breakUnderscore :: String -> [String]
breakUnderscore s
  =  case dropWhile (=='_') s of
       "" -> []
       s' -> w : breakUnderscore s''
             where (w, s'') = break (=='_') s'

-- fxn for generating a random expression, using \cap, \cup, and \setminus operators (as specified in the assignment sheet)
generateRandEx :: Int -> ChoiceTree SetExpr
generateRandEx i | i < 1
 = Branch [ Branch [Node (Var varName) | varName <- ["A","B","C"]] -- should I have options like (Cap (Var "A") (Var "A") ? (do this and document in comments, as a creative)
          ]
generateRandEx i
 = do { i' <- nodes [0..i-1]
        ;e1 <- generateRandEx i'
        ;e2 <- generateRandEx (i - i' - 1)
        ;opr <- nodes [Cap, Cup, SetMinus]
        ;return (opr e1 e2)
       }

setConversion :: [ChoiceTree ([Field], [Int])]
setConversion
 = [do { i <- nodes [1..3] 
         ; expr <- generateRandEx i
         ; return ([FMath(myShow expr)], [])-- what goes in this list of ints?
    }]

genProof :: [Int] -> Exercise -> Exercise
genProof order def 
 = def{ eQuestion = [ FText $"Here is an example proof, can you put it in the right order?"
                    , FIndented 1 [FMath "(x + y)^2"] -- starting point, from result of generateRandEx
                    , FReorder "proof"
                        (map ([step1,step2,step3,step4,step5] !!) order)
                    ]
      , eBroughtBy = ["Donia Tung & Mikio Obuchi"] }
 where 
    --  expr = randomSelect(setConversion!!0)
    --  proof = 
     step1 = [ FMath "=", FText "{ definition of taking a square }"
            , FIndented 1 [FMath "(x + y) \\cdot (x + y)"] ]
     step2 = [ FMath "=", FText "{ distributivity of multiplication over addition }"
            , FIndented 1 [FMath "x \\cdot (x + y) + y \\cdot (x + y)"] ]
     step3 = [ FMath "=", FText "{ distributivity of multiplication over addition (applied twice) }"
            , FIndented 1 [FMath "x \\cdot x + x \\cdot y + y \\cdot x + y \\cdot y"] ]
     step4 = [ FMath "=", FText "{ definition of taking a square }"
            , FIndented 1 [FMath "x^2 + x \\cdot y + y \\cdot x + y^2"] ]
     step5 = [ FMath "=", FText "{ combining equal values }"
            , FIndented 1 [FMath "x^2 + 2xy + y^2"] ]

-- -- for generating the actual text of the question displayed to client
-- setConvQuer :: ([Field],[Integer]) -> Exercise -> Exercise
-- setConvQuer (quer, _solution) exer 
--   = exer { eQuestion=[FText "This is thequestion:  "] ++ quer ++
--                      [FFieldMath "answer"]}
  

-- -- fxn for generating feedback to the users, based on their parsed input
-- setConvFeedback :: ([Field],[Integer]) -> Map.Map String String -> ProblemResponse -> ProblemResponse
-- setConvFeedback (quer, sol) mStrs defaultRsp 
--   = reTime $ case Map.lookup "answer" mStrs of
--       Just v -> markCorrect $ 
--                 defaultRsp {prFeedback = [FText("you entered" ++ show v)]}
--       Nothing -> markWrong $
--                 defaultRsp {prFeedback = [FText("wrong, you entered nothing")]}


----------- SHOWING SETEXPR AS STRINGS ------
prec :: SetExpr -> Int 
prec (Cup _ _) = 2
prec (Cap _ _) = 2
prec (SetMinus _ _) = 2
prec (Power _) = 1
prec (Wedge _ _ ) = 2
prec (Vee _ _ ) = 2
prec (In _) = 1
prec (NotIn _) = 1 
prec (Subset _) = 1
--          | Wedge SetExpr SetExpr   -- and, intersection
--               | Vee SetExpr SetExpr     -- or, union
--               | In SetExpr      -- element of 
--               | NotIn SetExpr   -- not an element of
--               | Subset SetExpr -- subset of

-- symb :: SetExpr -> String 
-- symb (Var a) = a
-- symb (Cup e1 e2) = myShow e1 ++ "\\cup" ++ myShow e2
-- symb (Cap e1 e2) = myShow e1 ++ "\\cap" ++ myShow e2
-- symb (SetMinus e1 e2) = myShow e1 ++  "\\setminus" ++ myShow e2
-- symb (Power e) = "\\P(" ++ myShow e ++ ")"
symb :: SetExpr -> String 
symb (Var a) = a
symb (Cup _ _) = "\\cup" 
symb (Cap _ _ ) = "\\cap" 
symb (SetMinus _ _) =  "\\setminus" 
-- symb (Power _) = "\\P(" ++ myShow e ++ ")"
symb (Wedge _ _ ) = "\\wedge"
symb (Vee _ _ ) = "\\vee"
symb (In _) = "e \\in"
symb (NotIn _) = "e \\notin"
symb (Subset _) = "e \\subseteq"

showSpace :: String
showSpace = " "

myShow :: SetExpr -> String
myShow (Var n) = n 
myShow (SetBuilder e) 
  = "\\left\\{ e | " ++ myShow e ++ "\\right\\}"
myShow e
  = "(for all " ++ vars ++ ") " ++ showsPrec' p e
    where p = prec e
          vars = list_to_string ( getVars e )
          --  vars = removeDuplicates (list_to_string ( getVars e ) )
   
showParen' :: Bool -> String -> String
showParen' p x = if p then
                "(" ++ x ++  ")"
                else x

showsPrec' :: Int -> SetExpr -> String
showsPrec' p (Var n) = n
showsPrec' p (Cap e1 e2)
  = showParen' (p < q) (showsPrec' q e1 ++ showSpace ++ 
        symb (Cap e1 e2) ++ showSpace ++ showsPrec' (q-1) e2)
    where q = prec (Cap e1 e2)
showsPrec' p (Cup e1 e2)
  = showParen' (p < q) (showsPrec' q e1 ++ showSpace ++ 
        symb (Cup e1 e2) ++ showSpace ++ showsPrec' (q-1) e2)
    where q = prec (Cup e1 e2)
showsPrec' p (SetMinus e1 e2)
  = showParen' (p < q) (showsPrec' q e1 ++ showSpace ++ 
        symb (SetMinus e1 e2) ++ showSpace ++ showsPrec' (q-1) e2)
    where q = prec (SetMinus e1 e2)
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

removeDuplicates :: (Eq a) => [a] -> [a]
removeDuplicates list = remDups list []

remDups :: (Eq a) => [a] -> [a] -> [a]
remDups [] _ = []
remDups (x:xs) list2
    | (x `elem` list2) = remDups xs list2
    | otherwise = x : remDups xs (x:list2)