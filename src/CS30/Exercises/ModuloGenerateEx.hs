{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.ModuloGenerateEx where


import qualified Data.Map as Map

import CS30.Data
import CS30.Exercises.Data
import CS30.Exercises.SetBasics.SolutionChecker

import CS30.Exercises.ModularArithmetic.ModuloProof
import CS30.Exercises.ModularArithmetic.ModuloParser

import Debug.Trace

-- -------------------------------------- Exercise Generation ------------------------------------

modProofEx :: ExerciseType
modProofEx = exerciseType "ModuloProof" "L??.?????"
            "Numbers: modulo n with non-monotonic operations"
            modProofs -- List of problems
            genProof -- present question as 'Exercise'
            genFeedback -- handle response as 'ProblemResponse'

modProofs :: [ChoiceTree ([Field], [Int])]
modProofs
 = [ do e <- (exprOfSize i ['a', 'b', 'c', 'd'])
        l <- getLawPermuts _arithmetic_laws
        traceM ("Expression = " ++ (show e))
        g <- (genGiven e) -- ChoiceTree Law
        traceM ("ABCDEFG = " ++  (show g))
        randomProof' e ([g]++l)
        -- randomProof' e l
    | i <- [7,9,11]]

-- ToDo revert constants
-- Filter single variable expressions    
exprOfSize :: Int -> [Char] -> ChoiceTree Expression
exprOfSize 1 xs = Branch [aFixed]
-- exprOfSize 1 xs = Branch [aCon,aFixed]
                 where 
                   -- aCon = nodes (map Con [0..4])
                   aFixed = nodes [Fixed nm | nm <- xs]
exprOfSize n xs
 = Branch [Branch [BinOp op <$> exprOfSize i xs<*> exprOfSize (n - i - 1) xs
                    | i <- [1..(n-2)], i `mod` 2 == 1] 
            | op <- [Pow, Mul, Sub, Add]]


genGiven :: Expression -> ChoiceTree Law
genGiven e = do expr <- (exprOfSize 3 (getRHS e)) --  = c * c
                return (Law "given" (Fixed (head (getVariables e)), expr))

getRHS :: Expression -> [Char]
getRHS e
  | length t == 0 = [x |x <- ['a', 'b', 'c', 'd'], not $ elem x vars]
  | otherwise = t
  where
    vars = getVariables e
    h = head vars
    t = tail vars

randomProof' :: Expression -> [Law] -> ChoiceTree ([Field], [Int])
randomProof' a lws
  | length (getPrfStep prf) == 0 = structurize <$> (getProofPermuts (proofToField ProofError))
  | otherwise = structurize  <$> (getProofPermuts (proofToField prf))
  where
    prf = getDerivation 5 lws a
    given = head lws
    getPrfStep (Proof _ lst) = lst
    getPrfStep ProofError = []
    structurize (x,y) = ([FText $"Can you put it in the right order?",
                                   FIndented 1 [FMath (show a)],
                                   FText $ "Given",
                                   FIndented 1 [FMath (show (fst (lawEq given))), FMath "=", FMath (show (snd (lawEq given)))],
                                   FReorder "proof" x], y)

proofToField :: Proof -> [[Field]]
proofToField ProofError = [[FText "No Proof was found for this expression"]]
proofToField (Proof exp steps) = trace ("Stepsssss = " ++ (show steps)) $ [ wrapField st | st <- steps]
  where
    wrapField (a,b) = [FMath "\\equiv_{p}", FText a, FIndented 1 [FMath (show b)]]

permutations :: Int -> ChoiceTree [Int]
permutations 0 = Node []
permutations n -- ChoiceTree is a Monad now! I've also derived "Show", so you can more easily check this out in GHCI.
 = do i <- nodes [0..n-1]
      rm <- permutations (n-1)
      return (i : map (\v -> if v >= i then v+1 else v) rm)


assignRandExprs :: Int -> [a] -> ChoiceTree [(a, Expression)]
assignRandExprs = undefined

sizeOf :: Expression -> Int
sizeOf = undefined

-- get atleast one step
-- smartForceLaw e siz
--   = do let vars = nubSort (getVariables e)
--        assignments <- assignRandExprs (siz - sizeOf e) vars
--        return (apply assignments e)

getProofPermuts :: [[Field]] -> ChoiceTree ([[Field]], [Int])
getProofPermuts x = do p <- permutations (length x)
                       return (map (x !!) p, p)

getLawPermuts :: [Law] -> ChoiceTree [Law]
getLawPermuts x = do p <- permutations (length x)
                     return (map (x !!) p)

genProof :: ([Field], [Int]) -> Exercise -> Exercise
genProof (quer, _solution) ex = trace ("Solution = " ++ (show _solution)) $  ex{eQuestion=quer, eBroughtBy = ["Sanket S. Joshi", "Anmol Chachra"] }

genFeedback :: ([Field], [Int]) -> Map.Map String String -> ProblemResponse -> ProblemResponse
genFeedback (_, sol) mStrs resp = trace ("Solution = " ++ (show sol)) $ 
                                  case Map.lookup "proof" mStrs of
                                    Just str
                                      -> trace ("\nResponse = " ++ (show (breakUnderscore str))) $ if map ((sol !!) . read) (breakUnderscore str) == [0..((length sol)-1)]
                                         then markCorrect resp
                                         else markWrong resp{prFeedback=[FText "You answered: ",FText str]}
                                    Nothing -> error "Client response is missing 'proof' field"

breakUnderscore :: String -> [String]
breakUnderscore s
  =  case dropWhile (=='_') s of
       "" -> []
       s' -> w : breakUnderscore s''
             where (w, s'') = break (=='_') s'

--------------------------------------Random Expression Generation-------------------------------

-- Takes a list of operations and fit them in between a,b,c,d
-- Example gen_expressions ['*', '+']
-- a + b + c + d
-- a + b + c * d
-- a + b * c + d
-- a + b * c * d
gen_expressions :: String -> [String]
gen_expressions [] = []
gen_expressions oper = [sbst op |op <- op_list]
    where
        op_list = [[i,j,k] | i <- oper, j <- oper, k <- oper]
        sbst x = (concat [[fst a, snd a] | a <- zip "abc" x]) ++ ['d']


-- Testing
-- expressions = gen_expressions [charAdd, charMul, charSub]
-- example =  parseExpression True ((gen_expressions [charAdd, charMul, charSub]) !! 22)
-- example_proof = getDerivation 5 arithmetic_laws example
-- example_fields = proofToField example_proof
-- sample = getProofPermuts example_fields