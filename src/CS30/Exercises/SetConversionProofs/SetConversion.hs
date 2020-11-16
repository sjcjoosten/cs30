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
import           Data.Aeson as JSON
import CS30.Exercises.SetConversionProofs.SetExprParser
import CS30.Exercises.SetConversionProofs.LawParser (parsedBasicLaws, parsedAdvancedLaws, Law(..))
import CS30.Exercises.SetConversionProofs.GenerateProof (Proof(..), generateProof)
import Data.Aeson.TH (deriveJSON)

-- data type for setexpr questions
data STEx = STEx {exprAsField::[Field],ord::[Int], lev::Int, expr::SetExpr} deriving (Show)
$(deriveJSON defaultOptions ''STEx)

-- setConv definition for export to Pages.hs
setConv :: ExerciseType
setConv = exerciseType "SetConversion" "L??" "Conversion to set-builder notation" 
              setConversion 
              genProof 
              feedback    

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
genBasicEx :: Int -> ChoiceTree SetExpr
genBasicEx i | i < 1
 = return (Var "A")
genBasicEx i
 = do i' <- nodes [0..i-1]
      e1 <- genBasicEx i'
      e2 <- genBasicEx (i - i' - 1)
      opr <- nodes [Cap, Cup, SetMinus]
      return (opr e1 e2) 

-- helper fxn for generating random expressions including powerset
binExprs :: (SetExpr -> SetExpr -> SetExpr) -> Int -> ChoiceTree SetExpr
binExprs operator i
 = do i' <- nodes [0..i-1]
      e1 <- genAdvEx i'
      e2 <- genAdvEx (i - i' - 1)
      return (operator e1 e2)

-- helper fxn for powerset random expressions
unaryExprs :: (SetExpr -> SetExpr) -> Int -> ChoiceTree SetExpr
unaryExprs operator i
 = do e1 <- genAdvEx (i -1)
      return (operator e1)

-- creative element: adds in Power set to random expression generation
genAdvEx :: Int -> ChoiceTree SetExpr
genAdvEx i | i < 1
 = return (Var "A")
genAdvEx i 
 = do result <- nodes [binExprs Cap i, binExprs Cup i, binExprs SetMinus i, 
                       unaryExprs Power i] 
      result

-- creative element: generates a ChoiceTree of SetExprs that are based on the format of the advanced laws
genSuperAdvEx :: [Law] -> ChoiceTree SetExpr
genSuperAdvEx laws 
    = do {  i <- nodes [0..1]
        ; let j = (1 `subtract` i)
        ; a <- genAdvEx i -- could make i or j or both 0, and replace (2 `subtract` i `subtract` j) with 0 too if we wanted a simpler expr
        ; b <- genAdvEx j
        ; c <- genAdvEx (2 `subtract` i `subtract` j)
        ; n <- nodes [0..4] -- because there are 5 advanced laws
        ; let (Law _nm (left, _rt)) = laws!!n
        ; let expr' = fst(assignVarAdv left [a, b, c])
        ; return expr'
        } 
                
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

-- specific assignVar definition for generating advanced expressions, replaces var with a given setexpr in the inputted list
assignVarAdv :: SetExpr -> [SetExpr] -> (SetExpr, [SetExpr]) 
assignVarAdv (Var _) [] = error "Problem in assigning variables!" -- if things work correctly, it should never reach this point 
assignVarAdv (Var _) (x:xs) = (x, xs)
assignVarAdv (Cup e1 e2) lst 
  = let (e1', lst') = assignVarAdv e1 lst 
        (e2', lst'') = assignVarAdv e2 lst'
    in (Cup e1' e2', lst'')
assignVarAdv (Cap e1 e2) lst 
  = let (e1', lst') = assignVarAdv e1 lst 
        (e2', lst'') = assignVarAdv e2 lst'
    in (Cap e1' e2', lst'')
assignVarAdv (SetMinus e1 e2) lst 
  = let (e1', lst') = assignVarAdv e1 lst 
        (e2', lst'') = assignVarAdv e2 lst'
    in (SetMinus e1' e2', lst'')
assignVarAdv (Vee e1 e2) lst 
  = let (e1', lst') = assignVarAdv e1 lst 
        (e2', lst'') = assignVarAdv e2 lst'
    in (Vee e1' e2', lst'')
assignVarAdv (Wedge e1 e2) lst 
  = let (e1', lst') = assignVarAdv e1 lst 
        (e2', lst'') = assignVarAdv e2 lst'
    in (Wedge e1' e2', lst'')
assignVarAdv (SetBuilder e) lst 
  = let (e', lst') = assignVarAdv e lst 
    in (SetBuilder e', lst')
assignVarAdv (In e) lst 
  = let (e', lst') = assignVarAdv e lst 
    in (In e', lst')
assignVarAdv (NotIn e) lst 
  = let (e', lst') = assignVarAdv e lst 
    in (NotIn e', lst')
assignVarAdv (Subset e) lst 
  = let (e', lst') = assignVarAdv e lst 
    in (Subset e', lst')
assignVarAdv (Power e) lst 
  = let (e', lst') = assignVarAdv e lst 
    in (Power e', lst')

-- creating the choice tree for problems, 3 levels of difficulty
-- creative element: the variation in problem type isn't just centered around number of expressions or size of problem 
setConversion :: [ChoiceTree STEx] 
setConversion
 = [
     Branch [ do { ex <- genBasicEx 2     -- level 1 --> simple cap, cup, set min 
                   ; let expr' = fst (assignVar ex ["A", "B", "C"]) 
                   ; let (Proof _expr steps) = generateProof parsedBasicLaws expr'
                   ; order <- removeRep ( permutations (length steps))
                   ;  return STEx {exprAsField = [FIndented 1 [FMath (myShow expr')], FReorder "proof" (map ((map showProofLine steps) !!) order)], ord = order, lev = 1, expr = expr'}
                 } ]
     , Branch [ do { ex <- genAdvEx i  -- level 2 --> add in power
                   ; let expr' = fst (assignVar ex ["A", "B", "C", "D"]) 
                   ; let (Proof _expr steps) = generateProof parsedBasicLaws expr'
                   ; order <- removeRep ( permutations (length steps))
                   ; return STEx {exprAsField = [FIndented 1 [FMath (myShow expr')], FReorder "proof" (map ((map showProofLine steps) !!) order)], ord = order, lev = 2, expr = expr'}
                 } | i <- [2..3]]
     , Branch [do { ex <- genSuperAdvEx parsedAdvancedLaws -- level 3 --> add in advanced laws, forces an expr that uses at least one of the advanced laws
                   ; let expr' = fst (assignVar ex ["A", "B", "C", "D", "E", "F", "G"]) 
                   ; let (Proof _expr steps) = generateProof parsedAdvancedLaws expr'
                   ; order <- permutations (length steps) -- SJC: this generates empty branches sometimes (because of zero/one-step proofs), so removing the removeRep here
                   ; return STEx {exprAsField = [FIndented 1 [FMath (myShow expr')], FReorder "proof" (map ((map showProofLine steps) !!) order)], ord = order, lev = 3, expr = expr'}
                 } ]
   ]

-- a simple mechanism for generating the choice tree for questions
-- setConversion :: [ChoiceTree ([Field], [Int])] 
-- setConversion
--  = [do { expr <- genAdvEx i
--          ; let expr' = fst (assignVar expr ["A", "B", "C", "D", "E"]) 
--          ; let (Proof _expr steps) = generateProof parsedBasicLaws expr'
--          ; order <- permutations (length steps)
--          ; return ([FIndented 1 [FMath (myShow expr')], FReorder "proof" (map ((map showProofLine steps) !!) order)], order)
--     } | i <- [2..4]]

----------- QUESTION ------

-- generating the proof question
genProof :: STEx -> Exercise -> Exercise
genProof (STEx {exprAsField = ex, ord = _o, lev = _l, expr = _ex}) def 
 = def{ eQuestion = [ FText $"Here is a proof, can you put it in the right order?"] ++ ex
      , eBroughtBy = ["Donia Tung", "Mikio Obuchi"] }

----------- FEEDBACK ------

-- feedback mechanism 
-- structure mostly taken from Proof.hs
feedback :: STEx -> Map.Map String String -> ProblemResponse -> ProblemResponse
feedback (STEx {exprAsField = _e, ord = order, lev = level, expr = ex}) rsp pr
               = reTime $ case Map.lookup "proof" rsp of
                   Just str
                     -> if length num == 0 
                        then markCorrect pr{prFeedback=[FText"Nice Job!"]}
                        else markWrong pr{prFeedback=[FText "You answered with the steps in the order: ",
                                                      FText (show (map ((order !!) . read) (breakUnderscore str))), 
                                                      FText". You had ",
                                                      FText (show (length num)),  -- creative element: gives the user a bit more information on their incorrect answer
                                                      FText " steps in the wrong order.", -- tells them how many lines they had in the wrong order and shows the correct proof
                                                      FIndented 0 ([FText "The correct proof would read: " ] ++ showProof proof)
                                                     ]}                          
                        where num = isRight 0 (map ((order !!) . read) (breakUnderscore str) )
                              proof = if level < 3 then generateProof parsedBasicLaws ex else generateProof parsedAdvancedLaws ex
                              -- a note: sometimes the advanced expressions generated are quite long, in which case with the current JS, the feedback modal cuts off the 
                              -- display of the correct proof. This would be fixed if the modal were made scrollable or alternatively by hardcoding the size of some the 
                              -- replacement variables, but I'll leave that choice up to you
                   Nothing -> error "Client response is missing 'proof' field"

-- fxn for showing the proof nicely 
showProof :: Proof -> [Field]
showProof (Proof start lst ) 
  = fstln ++ concatMap showProofLine lst
    where fstln = [FIndented 0 [], FMath( myShow start), FIndented 0 []]

-- translating a line in a proof into a field
showProofLine :: (String, SetExpr) -> [Field]
showProofLine (lwnm, ex)
  = [
      FMath "=", FText lwnm
      , FIndented 1 [FMath (myShow ex)]
    ] 

-- fxn for determining which, if any, steps were in the wrong order
isRight :: Int -> [Int] -> [(Int, Int)]
isRight _n [] = []
isRight n (x:xs) 
 = [(n,x) | n /= x ] ++ isRight (x+1) xs

-- fxn for counting if an ordering is consecutive
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
