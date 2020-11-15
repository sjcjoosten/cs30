{-# LANGUAGE TemplateHaskell #-}

{-
authors: Donia Tung & Mikio Obuchi 
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
import Debug.Trace


-- setConv definition for export to Pages.hs
setConv :: ExerciseType
setConv = exerciseType "Set Conversion" "L??" "Conversion to set-builder notation" 
              setConversion 
              genProof 
              feedback  

-- simple feedback mechanism 
-- structure mostly taken from Proof.hs
feedback :: ([Field],[Int]) -> Map.Map String String -> ProblemResponse -> ProblemResponse
feedback (problem, sol) rsp pr
               = reTime $ case Map.lookup "proof" rsp of
                   Just str
                     -> if length num == 0 
                        then markCorrect pr{prFeedback=[FText"Nice Job!"]}
                        else trace ( show( map showDetails ( Map.toList rsp)) )
                             trace ("intput order: " ++ show(map ((sol !!) . read) (breakUnderscore str) )  )
                             trace ("num result: " ++show num )
                             markWrong pr{prFeedback=[FText "You answered with the steps in the order: ",
                                                      FText (show (map ((sol !!) . read) (breakUnderscore str))), 
                                                      FText". You had ",
                                                      FText (show (length num)),  -- creative element: gives the user a bit more information on their incorrect answer
                                                      FText " steps in the wrong order :(", 
                                                      FText "The correct proof would read: "]}
                        where -- numWrong = wrongOrd (map ((sol !!) . read) (breakUnderscore str) ) 
                              num = isRight 0 (map ((sol !!) . read) (breakUnderscore str) ) 
                              -- proof = generateProof parsedBasicLaws problem
                   Nothing -> error "Client response is missing 'proof' field"

showDetails :: (String, String) -> String
showDetails (key, val) = key ++ ", " ++ val

-- to generate the proof in the user gets it OR correct order, check if step follows the one it's supposed to follow
-- could provide correct proof
-- isRight n (x:xs) | n == x 
--   = isRight (n+1) xs
--   | otherwise = [error] ++ isRight (x+1) xs
isRight :: Int -> [Int] -> [(Int, Int)]
isRight _n [] = []
isRight n (x:xs) 
 = [(n,x) | n /= x ] ++ isRight (x+1) xs

-- fxn for counting how many steps were in the wrong order
wrongOrd :: (Enum a, Num a, Eq a) => [a] -> Int
wrongOrd [] = 0
wrongOrd lst = length ( filter (\(x, y) -> x /= y) zipped ) 
    where zipped = zip lst [0..]

-- from ProofStub.hs
breakUnderscore :: String -> [String]
breakUnderscore s
  =  case dropWhile (=='_') s of
       "" -> []
       s' -> w : breakUnderscore s''
             where (w, s'') = break (=='_') s'

-- from ProofStub.hs
permutations :: Int -> ChoiceTree [Int]
permutations 0 = Node []
permutations n -- ChoiceTree is a Monad now! I've also derived "Show", so you can more easily check this out in GHCI.
 = do i <- nodes [0..n-1]
      rm <- permutations (n-1)
      return (i : map (\v -> if v >= i then v+1 else v) rm)

-- creative element: removes the permutation of orderings that's in the correct order
-- ensures the user always has to rearrange something
removeRep :: ChoiceTree [Int] -> ChoiceTree [Int]
removeRep (Node lst) 
  = if wrongOrd lst /= 0 then (Node lst)
    else (Node [])
removeRep (Branch (x:xs))
  = if removeRep x == (Node []) || removeRep x == (Branch []) then Branch xs
    else Branch (map removeRep (x:xs)) 
removeRep (Branch [])
  = error "should never get here because Branch should always have a Node inside, or gets caught by above case"

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

-- helper fxn for generating random expressions including powerset
binExprs :: (SetExpr -> SetExpr -> SetExpr) -> Int -> ChoiceTree SetExpr
binExprs operator i
 = do i' <- nodes [0..i-1]
      e1 <- genRand i'
      e2 <- genRand (i - i' - 1)
      return (operator e1 e2)

-- helper fxn for powerset random expressions
unaryExprs :: (SetExpr -> SetExpr) -> Int -> ChoiceTree SetExpr
unaryExprs operator i
 = do e1 <- genRand (i -1)
      return (operator e1)

-- creative element: adds in Power set to random expression generation
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
-- creative element: ensures that there is no duplication of variables in the random expr generated as the problem
assignVar :: SetExpr -> [String] -> (SetExpr, [String]) 
assignVar (Var _) [] = error "Problem in assigning variables!" -- if things work correctly, it should never reach this point 
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


-- creating the choice tree for problems, 3 levels of difficulty
-- creative element: the variation in problem type isn't just centered around number of expressions or size of problem 
setConversion :: [ChoiceTree ([Field], [Int])] 
setConversion
 = [
     Branch [ do { expr <- generateRandEx 2     -- level 1 --> simple cap, cup, set min 
                   ; let expr' = fst (assignVar expr ["A", "B", "C"]) 
                   ; let (Proof _expr steps) = generateProof parsedBasicLaws expr'
                   ; order <- removeRep ( permutations (length steps))
                   ;  return ([FIndented 1 [FMath (myShow expr')], FReorder "proof" (map ((map showProofLine steps) !!) order)], order)
                 } ]
     , Branch [ do { expr <- genRand i  -- level 2 --> add in power
                   ; let expr' = fst (assignVar expr ["A", "B", "C", "D"]) 
                   ; let (Proof _expr steps) = generateProof parsedBasicLaws expr'
                   ; order <- removeRep ( permutations (length steps))
                   ;  return ([FIndented 1 [FMath (myShow expr')], FReorder "proof" (map ((map showProofLine steps) !!) order)], order)
                 } | i <- [2..3]]
     , Branch [do { expr <- genRand i   -- level 3 --> add in advanced laws
                   ; let expr' = fst (assignVar expr ["A", "B", "C", "D"]) 
                   ; let (Proof _expr steps) = generateProof parsedAdvancedLaws expr'
                   ; order <- removeRep ( permutations (length steps))
                   ;  return ([FIndented 1 [FMath (myShow expr')], FReorder "proof" (map ((map showProofLine steps) !!) order)], order)
                 } | i <- [2..3] ]
   ]

-- this was a different mechanism for generating the choice tree for questions
-- setConversion :: [ChoiceTree ([Field], [Int])] 
-- setConversion
--  = [do { expr <- genRand i
--          ; let expr' = fst (assignVar expr ["A", "B", "C", "D", "E"]) 
--          ; let (Proof _expr steps) = generateProof parsedBasicLaws expr'
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
