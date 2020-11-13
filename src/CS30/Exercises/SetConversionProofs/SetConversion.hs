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
import CS30.Exercises.SetConversionProofs.LawParser (parsedBasicLaws, parsedAdvancedLaws)
import CS30.Exercises.SetConversionProofs.GenerateProof (Proof(..), generateProof)


-- setConv definition for export to Pages.hs
setConv :: ExerciseType
setConv = exerciseType "Set Conversion" "L??" "Conversion to set-builder notation" 
              setConversion 
              genProof 
              simpleFeedback  

-- simple feedback mechanism 
simpleFeedback :: ([Field],[Int]) -> Map.Map [Char] String -> ProblemResponse -> ProblemResponse
simpleFeedback (_problem, sol) rsp pr
               = reTime $ case Map.lookup "proof" rsp of
                   Just str
                     -> if numWrong == 0 
                        then markCorrect pr
                        else markWrong pr{prFeedback=[FText "You answered with the steps in the order: ",
                                                      FText str, 
                                                      FText". You had ",
                                                      FText (show numWrong),  -- creative element: gives the user a bit more information on their incorrect answer
                                                      FText " steps in the wrong order :("]}
                        where numWrong = wrongOrd (map ((sol !!) . read) (breakUnderscore str) ) 
                   Nothing -> error "Client response is missing 'proof' field"

-- fxn for determining if a list is comprised of directly successive numbers for super simple response checking 
-- isSucc :: (Enum a, Eq a) => [a] -> Bool
-- isSucc [] = True
-- isSucc (_x:[]) = True
-- isSucc (x:y:zs) | y == succ x = isSucc $ y:zs

-- fxn for counting how many steps were in the wrong order
wrongOrd :: (Enum a, Num a, Eq a) => [a] -> Int
wrongOrd [] = 0
wrongOrd lst = length ( filter (\(x, y) -> x /= y) (zip lst [0..]) ) 



-- wrongOrd :: (Enum a, Num a, Eq a) => [a] -> Int -> Int
-- wrongOrd [] _ = 0
-- wrongOrd [_x] n = n  
-- wrongOrd (x:xs) n = if (x + 1) == head xs 
--                     then wrongOrd xs n  
--                     else wrongOrd xs (n + 1)

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
 = Branch [ Branch [Node (Var varName) | varName <- ["A"]] 
          ]
generateRandEx i
 = do { i' <- nodes [0..i-1]
        ;e1 <- generateRandEx i'
        ;e2 <- generateRandEx (i - i' - 1)
        ;opr <- nodes [Cap, Cup, SetMinus]
        ;return (opr e1 e2) 
       }
       
binExprs :: (SetExpr -> SetExpr -> SetExpr) -> Int -> ChoiceTree SetExpr
binExprs operator i
 = do i' <- nodes [0..i-1]
      e1 <- genRand i'
      e2 <- genRand (i - i' - 1)
      return (operator e1 e2)

unaryExprs :: (SetExpr -> SetExpr) -> Int -> ChoiceTree SetExpr
unaryExprs operator i
 = do e1 <- genRand (i -1)
      return (operator e1)

genRand :: Int -> ChoiceTree SetExpr
genRand i | i < 1
 = Branch [ Branch [Node (Var varName) | varName <- ["A"]] 
          ]
genRand i 
 = do result <- nodes [binExprs Cap i, binExprs Cup i, binExprs SetMinus i, 
                       unaryExprs Power i] 
      result
-- join combines two trees together 
-- same as 
-- genRand i
--  = join (nodes [binExprs Cap, binExprs Cup, binExprs SetMinus, 
--                        unaryExprs Power] )

-- assignVar definition to go over the final expression by replacing all variables from left to right.
-- creative element; ensures that there is no duplication of variables in the random expr generated as the problem
assignVar :: SetExpr -> [String] -> (SetExpr, [String]) 
-- assignVar (Var _) [] = () -- TODO: how to define this
assignVar (Var _) (x:xs) = (Var x, xs)
assignVar (Cup e1 e2) lst 
  = let (e1', lst') = assignVar e1 lst 
        (e2', lst'') = assignVar e2 lst'
    in (Cup e1' e2', lst'')
assignVar (Cap e1 e2) lst 
  = let (e1', lst') = assignVar e1 lst 
        (e2', lst'') = assignVar e2 lst'
    in (Cap e1' e2', lst'')
assignVar (SetMinus e1 e2) lst 
  = let (e1', lst') = assignVar e1 lst 
        (e2', lst'') = assignVar e2 lst'
    in (SetMinus e1' e2', lst'')
assignVar (Vee e1 e2) lst 
  = let (e1', lst') = assignVar e1 lst 
        (e2', lst'') = assignVar e2 lst'
    in (Vee e1' e2', lst'')
assignVar (Wedge e1 e2) lst 
  = let (e1', lst') = assignVar e1 lst 
        (e2', lst'') = assignVar e2 lst'
    in (Wedge e1' e2', lst'')
assignVar (SetBuilder e) lst 
  = let (e', lst') = assignVar e lst 
    in (SetBuilder e', lst')
assignVar (In e) lst 
  = let (e', lst') = assignVar e lst 
    in (In e', lst')
assignVar (NotIn e) lst 
  = let (e', lst') = assignVar e lst 
    in (NotIn e', lst')
assignVar (Subset e) lst 
  = let (e', lst') = assignVar e lst 
    in (Subset e', lst')
assignVar (Power e) lst 
  = let (e', lst') = assignVar e lst 
    in (Power e', lst')

-- could change the laws based on level
-- first case, diff var names
-- last case allow duplicates

-- level 1 --> simple cap, cup, set min 
-- level 2 --> add in power
-- level 3 --> add in hard laws
-- level 4 --> duplicate var names

-- TODO: add in check for order that it's not already in order
-- that can be another creative element
setConversion :: [ChoiceTree ([Field], [Int])] 
setConversion
 = [
     Branch [ do { expr <- generateRandEx 2
                   ; let expr' = fst (assignVar expr ["A", "B", "C"]) 
                   ; let (Proof _expr steps) = generateProof parsedBasicLaws expr'
                   ; order <- permutations (length steps)
                   ;  return ([FIndented 1 [FMath (myShow expr')], FReorder "proof" (map ((map showProofLine steps) !!) order)], order)
                 } ]
     , Branch [ do { expr <- genRand i
                   ; let expr' = fst (assignVar expr ["A", "B", "C", "D"]) 
                   ; let (Proof _expr steps) = generateProof parsedBasicLaws expr'
                   ; order <- permutations (length steps)
                   ;  return ([FIndented 1 [FMath (myShow expr')], FReorder "proof" (map ((map showProofLine steps) !!) order)], order)
                 } | i <- [2..3]]
     , Branch [do { expr <- genRand i
                   ; let expr' = fst (assignVar expr ["A", "B", "C", "D"]) 
                   ; let (Proof _expr steps) = generateProof parsedAdvancedLaws expr'
                   ; order <- permutations (length steps)
                   ;  return ([FIndented 1 [FMath (myShow expr')], FReorder "proof" (map ((map showProofLine steps) !!) order)], order)
                 } | i <- [2..3] ]
   ]

-- creating the choice tree for problems, 3 levels in terms of degree of nested expr
-- setConversion :: [ChoiceTree ([Field], [Int])] 
-- setConversion
--  = [do { expr <- genRand i
--          ; let expr' = fst (assignVar expr ["A", "B", "C", "D", "E"]) 
--          ; let (Proof _expr steps) = generateProof parsedBasicLaws expr'
--          ; order <- permutations (length steps)
--          ; return ([FIndented 1 [FMath (myShow expr')], FReorder "proof" (map ((map showProofLine steps) !!) order)], order)
--     } | i <- [3..4]]
-- setConversion :: [ChoiceTree ([Field], [Int])] 
-- setConversion
--  = [do { expr <- genRand i
--          ; let expr' = fst (assignVar expr ["A", "B", "C", "D", "A"]) -- so that when we get to the third level, there are repeated var to deal with 
--          ; let (Proof _expr steps) = if i < 3 then generateProof parsedBasicLaws expr' else generateProof parsedAdvancedLaws expr' 
--          ; order <- permutations (length steps)
--          ; return ([FIndented 1 [FMath (myShow expr')], FReorder "proof" (map ((map showProofLine steps) !!) order)], order)
--     } | i <- [2..4]]


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
-- I ended up keeping this because it was used in myShow, in which I don't enumerate all the various setexpr cases
prec (Var _) = 0
prec (Power _) = 0
prec (SetBuilder _) = 1
prec (Subset _) = 1
prec (Vee _ _) = 2 
prec (Wedge _ _) = 2 
prec (In  _) = 5
prec (NotIn _) = 5
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
showsPrec' p (In e) 
  = showParen' (p > q) ("e \\in" ++ showSpace ++ showsPrec' (q+1) e)
    where q = 5
showsPrec' p (NotIn e)
  = showParen' (p > q) ( "e \\notin" ++ showSpace ++ showsPrec' (q+1) e)
    where q = 5
showsPrec' p (Cap e1 e2)
  = showParen' (p > q) (showsPrec' q e1 ++ showSpace ++ 
        "\\cap" ++ showSpace ++ showsPrec' (q+1) e2)
    where q = 4
showsPrec' p (Cup e1 e2)
  = showParen' (p > q) (showsPrec' q e1 ++ showSpace ++ 
        "\\cup" ++ showSpace ++ showsPrec' (q+1) e2)
    where q = 4
showsPrec' p (SetMinus e1 e2)
  = showParen' (p > q) (showsPrec' q e1 ++ showSpace ++ 
        "\\setminus" ++ showSpace ++ showsPrec' (q+1) e2)
    where q = 4
showsPrec' p (Wedge e1 e2)
  = showParen' (p > q) (showsPrec' q e1 ++ showSpace ++ 
        "\\wedge" ++ showSpace ++ showsPrec' (q+1) e2)
    where q = 2
showsPrec' p (Vee e1 e2)
  = showParen' (p > q) (showsPrec' q e1 ++ showSpace ++ 
         "\\vee" ++ showSpace ++ showsPrec' (q+1) e2)
    where q = 2
showsPrec' p (Subset e)
  =  showParen' (p > q) ( "e \\subseteq" ++ showSpace ++ showsPrec' (q+1) e)
    where q = 1
showsPrec' _p (Power e)
  = "\\P\\left("  ++ showsPrec' (0) e ++ "\\right)"
showsPrec' p (SetBuilder e) 
  = showParen' (p > q) ( "\\left\\{ e | " ++ myShow e ++ "\\right\\}")
    where q = 1
