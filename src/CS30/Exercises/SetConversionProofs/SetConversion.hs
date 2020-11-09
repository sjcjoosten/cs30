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
import qualified Data.Map as Map
import CS30.Exercises.SetConversionProofs.SetExprParser
import CS30.Exercises.SetConversionProofs.LawParser (parsedLaws)
import CS30.Exercises.SetConversionProofs.GenerateProof (Proof(..), generateProof)


-- setConv definition for export to Pages.hs
setConv :: ExerciseType
setConv = exerciseType "SetConversion" "L??" "Conversion to set-builder notation" 
              setConversion 
              genProof 
              simpleFeedback  

-- simple feedback mechanism 
simpleFeedback :: ([Field],[Int]) -> Map.Map [Char] String -> ProblemResponse -> ProblemResponse
simpleFeedback (_problem, sol) rsp pr
               = reTime $ case Map.lookup "proof" rsp of
                   Just str
                     -> if isSucc (map ((sol !!) . read) (breakUnderscore str) )
                        then markCorrect pr
                        else markWrong pr{prFeedback=[FText "You answered: ",FText str]}
                   Nothing -> error "Client response is missing 'proof' field"

-- fxn for determining if a list is comprised of directly successive numbers
isSucc :: (Enum a, Eq a) => [a] -> Bool
isSucc [] = True
isSucc (_x:[]) = True
isSucc (x:y:zs) | y == succ x = isSucc $ y:zs
isSucc _ = False

-- from ProofStub.hs
permutations :: Int -> ChoiceTree [Int]
permutations 0 = Node []
permutations n -- ChoiceTree is a Monad now! I've also derived "Show", so you can more easily check this out in GHCI.
 = do i <- nodes [0..n-1]
      rm <- permutations (n-1)
      return (i : map (\v -> if v >= i then v+1 else v) rm)

-- from ProofStub.hs
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

-- creating the choice tree for problems, 3 levels in terms of degree of nested expr
setConversion :: [ChoiceTree ([Field], [Int])] 
setConversion
 = [do { expr <- generateRandEx i
         ; let (Proof _expr steps) = generateProof parsedLaws expr
         ; order <- permutations (length steps)
         ; return ([FIndented 1 [FMath (myShow expr)], FReorder "proof" (map ((map showProofLine steps) !!) order)], order)
    } | i <- [1..3]]

-- inverse permutation, checking in the other code 

-- generating the proof question
genProof :: ([Field],[Int]) -> Exercise -> Exercise
genProof (problem, _order) def 
 = def{ eQuestion = [ FText $"Here is a proof, can you put it in the right order?"] ++ problem
      , eBroughtBy = ["Donia Tung", "Mikio Obuchi"] }


-- translating a line in a proof into a field
showProofLine :: (String, SetExpr) -> [Field]
showProofLine (lwnm, expr)
  = [
      FMath "=", FText lwnm
      , FIndented 1 [FMath (myShow expr)]
    ] 

----------- SHOWING SETEXPR AS STRINGS ------
prec :: SetExpr -> Int 
-- I ended up keeping this because it was used in myShow, in which I don't enumerate all the various 
-- setexpr cases, but if there is a way to get rid of it without having to entirely rewrite that function, I can do that
prec (Var _) = 0
prec (Power _) = 1
prec (SetBuilder _) = 1
prec (Subset _) = 1
prec (Vee _ _) = 2 
prec (Wedge _ _) = 2 
prec (In _) = 3
prec (NotIn _) = 3 
prec (Cap _ _) = 4
prec (Cup _ _) = 4
prec (SetMinus _ _) = 4

showSpace :: String
showSpace = " "

myShow :: SetExpr -> String
myShow (Var n) = n 
myShow e 
  = showsPrec' p e
    where p = prec e

showParen' :: Bool -> String -> String
showParen' p x = if p then
                "(" ++ x ++  ")"
                else x

showsPrec' :: Int -> SetExpr -> String
showsPrec' _p (Var n) = n
showsPrec' p (Cap e1 e2)
  = showParen' (p < q) (showsPrec' q e1 ++ showSpace ++ 
        "\\cap" ++ showSpace ++ showsPrec' (q-1) e2)
    where q = 4
showsPrec' p (Cup e1 e2)
  = showParen' (p < q) (showsPrec' q e1 ++ showSpace ++ 
        "\\cup" ++ showSpace ++ showsPrec' (q-1) e2)
    where q = 4
showsPrec' p (SetMinus e1 e2)
  = showParen' (p < q) (showsPrec' q e1 ++ showSpace ++ 
        "\\setminus" ++ showSpace ++ showsPrec' (q-1) e2)
    where q = 4
showsPrec' p (Wedge e1 e2)
  = showParen' (p < q) (showsPrec' q e1 ++ showSpace ++ 
        "\\wedge" ++ showSpace ++ showsPrec' (q-1) e2)
    where q = 2
showsPrec' p (Vee e1 e2)
  = showParen' (p < q) (showsPrec' q e1 ++ showSpace ++ 
         "\\vee" ++ showSpace ++ showsPrec' (q-1) e2)
    where q = 2
showsPrec' p (In e)
  = showParen' (p < q) ( "e \\in" ++ showSpace ++ showsPrec' (q-1) e)
    where q = 3
showsPrec' p (NotIn e)
  = showParen' (p < q) ( "e \\notin" ++ showSpace ++ showsPrec' (q-1) e)
    where q = 3
showsPrec' p (Subset e)
  = showParen' (p < q) ( "e \\subseteq" ++ showSpace ++ showsPrec' (q-1) e)
    where q = 1
showsPrec' p (Power e)
  = showParen' (p < q) ( "\\P" ++ showsPrec' (0) e)
    where q = 1
showsPrec' p (SetBuilder e) 
  = showParen' (p < q) ( "\\left\\{ e | " ++ myShow e ++ "\\right\\}")
    where q = 1
