module CS30.Exercises.GenerateExerViaProofs.ExpreParser where
import CS30.Exercises.GenerateExerViaProofs.ProofProbability
import CS30.Exercises.GenerateExerViaProofs.GenerateProof
import CS30.Exercises.Data
import CS30.Data
import Data.Ratio
import GHC.Show ( showSpace )

probabilityProof :: ExerciseType
probabilityProof 
    = exerciseType  "probabilityProof"
                    "L?.?"
                    "Probability: generating exercises via proofs" 
                    [genExercise 3]
                    genExer 
                    generateFeedback

{- Pr[A|(B|C)] = (P[A] + P[B|C]) - P[A & B|C] 
Given P[A] = 1/2, P[B|C] = 1/2, p[A & B|C] = 1/3
Calculate Pr[A|(B|C)]
The right answer would be 2/3
-}
genExercise :: Int -> ChoiceTree ((FracExpr, Rational), [(FracExpr,[Field], Rational)])
genExercise i = do expr <- probExpr i
                   let proof = getDerivation rules expr
                   let givenExpr = getLast proof
                   rList <- permutations 16
                   let givens = combineGiveExpr givenExpr rList
                   return ((expr ,randList (evaluate expr) rList), givens)

getField :: [[Field]]
getField = error "not implemented"
               
randList :: Val -> [Integer] -> Rational
randList (Chance f) lst = f lst

permutations :: Integer -> ChoiceTree [Integer]
permutations 0 = Node []
permutations l
  = do {i <- nodes [1..16]
        ;rm <- permutations (l-1)
        ;return (i : rm)
      }


combineGiveExpr :: [FracExpr] -> [Integer] -> [(FracExpr,[Field], Rational)]
combineGiveExpr exprlist rlst = [(e1,[FFieldMath "answer"], f rlst)| e1 <- exprlist, Chance f <- [evaluate e1]]

data Val = Evet [Bool] 
          |Chance ([Integer] -> Rational)

-- Pr[A|B|C|D]
evaluate :: FracExpr -> Val
evaluate (FVar 'A') = Evet (replicate 8 True ++ replicate 8 False)
evaluate (FVar 'B') = Evet (replicate 4 True ++ replicate 4 False ++ replicate 4 True ++ replicate 4 False)
evaluate (FVar 'C') = Evet (concat (replicate 4 [True, True, False, False]))
evaluate (FVar 'D') = Evet (concat (replicate 8 [True, False]))
evaluate (FVar var ) = error ("Unexpected Variables" ++ show var)
evaluate Omega = Evet (replicate 16 True)
evaluate EmptySet = Evet (replicate 16 False)
evaluate (AndEvent e1 e2) = let Evet e1v = evaluate e1
                                Evet e2v = evaluate e2
                            in  Evet (zipWith (&&) e1v e2v)
evaluate (OrEvent e1 e2) = let Evet ev1 = evaluate e1
                               Evet ev2 = evaluate e2
                            in  Evet (zipWith (||) ev1 ev2)
evaluate (NegaEvent e) = let Evet ev1 = evaluate e
                         in Evet(map not ev1)
evaluate (FConst var) = Chance (const var)
evaluate (FBin op e1 e2) = let Chance e1v = evaluate e1
                               Chance e2v = evaluate e2 
                               res lst = f (e1v lst) (e2v lst)
                            in 
                                Chance res
                            where f a b = case op of {Plus -> a + b; Minus -> a-b ; 
                                                      Mult -> a * b; Divide -> a/b}
                       
evaluate (Prob p) = let Evet ev1 = evaluate p
                    in Chance (getTrueRational ev1)


getTrueRational :: [Bool] -> [Integer] -> Rational
getTrueRational boolst intlst = 
    let tsum = getTrueRationalhelper boolst intlst in
        toRational (tsum `div` sum intlst)

getTrueRationalhelper :: Num p => [Bool] -> [p] -> p
getTrueRationalhelper [] [] = 0
getTrueRationalhelper (x:xs) (a:rst) = 
    if x then a+  getTrueRationalhelper xs rst else getTrueRationalhelper xs rst
getTrueRationalhelper _ _ = error "lst should have same length"

getLast :: Proof -> [FracExpr]
getLast (Proof expTemp []) = extractExpr expTemp
getLast (Proof _ steps) = extractExpr (snd (last steps))


extractExpr :: FracExpr -> [FracExpr]
extractExpr (Prob a) = [Prob a]
extractExpr (FBin _ a b) = extractExpr a ++ extractExpr b
extractExpr _ = []

-- Fvar 'A' 
-- Fvar 'B' 
-- A|B
-- generate problems for tests
generateRandEx :: Int -> ChoiceTree FracExpr
generateRandEx i 
    | i < 1 = Branch [Node (FVar varName) | varName <-['A','B','C','D']]    
generateRandEx i 
    = Branch[Branch [do {e1 <- generateRandEx i';           
                   e2 <- generateRandEx (i - i' - 1);        
                   return (OrEvent e1 e2)}    
                | i' <- [0..i-1]         
                ]
            ,Branch  [do {e1 <- generateRandEx i';           
                   e2 <- generateRandEx (i - i' - 1);        
                   return (AndEvent e1 e2)}    
                | i' <- [0..i-1]         
                ]
            ,do {e1 <- generateRandEx (i-1);           
                   return (NegaEvent e1)}    
            ]

probExpr :: Int -> ChoiceTree FracExpr
probExpr n = fmap Prob (generateRandEx n)

getBaseExpr :: [Law] -> FracExpr -> [FracExpr]
getBaseExpr lawss e = 
    let (Proof e' steps) = getDerivation lawss e in 
    if null steps then decomposeExpr e' 
    else getBaseExpr lawss (snd (head steps))
    
decomposeExpr :: FracExpr -> [FracExpr]
decomposeExpr e1 = 
            case e1 of 
                FBin _ ex1 ex2 -> decomposeExpr ex1 ++ decomposeExpr ex2
                _ -> [e1]


genExer :: ((FracExpr, Rational), [(FracExpr,[Field], Rational)]) -> Exercise -> Exercise
genExer ((expr, _answer), [(expr1, fid, answer)]) exercise 
    = exercise {eQuestion = [FText "Given " ] ++ processGiven [(expr1,answer)] ++ 
        [FIndented 0 [FText "Calculate ", FMath (myShow expr)], FFieldMath "answer"]}

processGiven :: [(FracExpr,Rational)] -> [Field]
processGiven list = combine [FMath $ myShow e1 ++ "=" ++ rationToLatex e2 | (e1, e2) <- list]

combine :: [Field] -> [Field]
combine [] = []
combine [v] = [v]
combine (as : ass) = [as] ++ [FText ", "] ++ combine ass 

rationToLatex :: Rational -> [Char]
rationToLatex 0 = "0"
rationToLatex 1 = "1"
rationToLatex r = "\\frac{" ++ show (numerator r) ++ "}" ++ "{" ++ show (denominator r) ++ "}"

generateFeedback = undefined

--------------------------deal with show-----------------------------------------------------------
prec :: MathOp -> Int
prec Mult = 2
prec Divide = 2
prec Minus = 1
prec Plus = 1


symb :: MathOp -> String
symb Mult = "\\cdot"
symb Divide = "/"
symb Plus = "+"
symb Minus = "-"

myShow :: FracExpr -> String
myShow expr = showsPrecHelper 3 expr "" 

showsPrecHelper :: Int -> FracExpr -> ShowS
showsPrecHelper _ (FConst n) = showString (show n)
showsPrecHelper _ (FVar n1) = showString [n1]
showsPrecHelper p (FBin op e1 e2)
        = showParen (p>q) (showsPrecHelper q e1 . showSpace .
        showString(symb op) . showSpace . showsPrecHelper (q+1) e2) where q = prec op
showsPrecHelper p (AndEvent e1 e2)
        = showsPrecHelper p e1 . showSpace .
        showString "\\wedge" . showSpace . showsPrecHelper p e2
showsPrecHelper p (OrEvent e1 e2)
        = showsPrecHelper p e1 . showSpace .
        showString "\\vee" . showSpace . showsPrecHelper p e2
showsPrecHelper _ Omega
        = showSpace . showString "\\Omega" . showSpace 
showsPrecHelper _ EmptySet
        = showSpace . showString "\\emptyset" . showSpace 
showsPrecHelper p (NegaEvent e1)
        =  showString "\\neg" . showSpace. showsPrecHelper p e1 . showSpace
showsPrecHelper p (Prob e1)
        =  showString "Pr[" . showSpace. showsPrecHelper p e1 . showString "]". showSpace



        