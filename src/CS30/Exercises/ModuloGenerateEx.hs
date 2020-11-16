{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.ModuloGenerateEx where

import qualified Data.Map as Map

import CS30.Data
import CS30.Exercises.Data

import CS30.Exercises.ModularArithmetic.ModuloProof
import CS30.Exercises.ModularArithmetic.ModuloParser


---------------------------------------- Exercise Generation ------------------------------------

-- main Exercise Generaion function 
modProofEx :: ExerciseType
modProofEx = exerciseType "ModuloProof" "L??.?????"
            "Numbers: modulo n with non-monotonic operations"
            modProofs -- List of problems
            genProof -- present question as 'Exercise'
            genFeedback -- handle response as 'ProblemResponse'

-- generates a list of choiceTree of (proofs rendered as list of Field, solution)
modProofs :: [ChoiceTree ([Field], [Int])]
modProofs
 = [ do e <- (exprOfSize i ['a', 'b', 'c', 'd'])
        l <- getLawPermuts _arithmetic_laws
        -- traceM ("Expression = " ++ (show e))
        g <- (genGiven e) -- ChoiceTree Law
        -- traceM ("ABCDEFG = " ++  (show g))
        randomProof' e ([g]++l)
    | i <- [7,9,11]]

-- Filter single variable expressions    
exprOfSize :: Int -> [Char] -> ChoiceTree Expression
exprOfSize 1 xs = Branch [aCon,aFixed]
                 where 
                   aCon = nodes (map Con [0..4])
                   aFixed = nodes [Fixed nm | nm <- xs]
exprOfSize n xs
 = Branch [Branch [BinOp op <$> exprOfSize i xs<*> exprOfSize (n - i - 1) xs
                    | i <- [1..(n-2)], i `mod` 2 == 1] 
            | op <- [Pow, Mul, Sub, Add]]

-- generates a choice tree of given laws - example given: a = b + c
genGiven :: Expression -> ChoiceTree Law
genGiven e = do expr <- (exprOfSize 3 (getRHS e))
                return (Law "Given" (Fixed (head (getVariablesWithFallback e)), expr))

-- helper function used in genGiven to make sure the rhs variables are not same as the lhs 
getRHS :: Expression -> [Char]
getRHS e
  | length t == 0 = [x |x <- ['a', 'b', 'c', 'd'], not $ elem x vars]
  | otherwise = t
  where
    vars = getVariablesWithFallback e
    t = tail vars

-- Takes an expression and a list of laws and generates choice tree of (proofs rendered as list of Field, solution)
randomProof' :: Expression -> [Law] -> ChoiceTree ([Field], [Int])
randomProof' a lws
  | length (getPrfStep prf) == 0 = structurize <$> (getProofPermuts (proofToField ProofError))
  | otherwise = structurize <$> (getProofPermuts (proofToField prf))
  where
    prf = getDerivation 5 lws a
    given = head lws
    getPrfStep (Proof _ lst) = lst
    getPrfStep ProofError = []
    structurize (x,y)

      | length (getPrfStep prf) > 0 = ([FText $ "Given : ",
                                        FIndented 1 [(FMath . show . fst . lawEq) given,
                                                     FMath "\\ =\\ ",
                                                     (FMath . show . snd . lawEq) given],
                                        FText $ "To prove : ",
                                        FIndented 1 [(FMath . show) a,
                                                     FMath "\\ \\equiv_{p}\\ \\ ",
                                                     (FMath . show . snd . last . getPrfStep) prf],
                                        FText $ "Can you put the proof in the right order?",
                                        FIndented 1 [FMath (show a)],
                                        FReorder "proof" x
                                      ], y)
      | otherwise = ([FText $ "Given : ",
                      FIndented 1 [(FMath . show . fst . lawEq) given, 
                                   FMath "\\ =\\ ", 
                                   (FMath . show . snd . lawEq) given],
                      FText $ "Expression : ",
                      FIndented 1 [(FMath . show) a],
                      FReorder "proof" x
                    ], y)

-- renders a Proof to a list of list of fields
-- used in randomProof' to structurize the generated Proof into what will be rendered in the front end
proofToField :: Proof -> [[Field]]
proofToField ProofError = [[FText "No Proof was found for this expression - Please click on Check to get a new exercise. Sorry for the inconvenience."]]
proofToField (Proof _ steps) = [ wrapField st | st <- steps]
  where
    wrapField (a,b) = [FMath "\\equiv_{p}\\space\\space", FText a, FIndented 2 [FMath (show b)]]

-- choice tree of permutations
permutations :: Int -> ChoiceTree [Int]
permutations 0 = Node []
permutations n -- ChoiceTree is a Monad now! I've also derived "Show", so you can more easily check this out in GHCI.
 = do i <- nodes [0..n-1]
      rm <- permutations (n-1)
      return (i : map (\v -> if v >= i then v+1 else v) rm)

-- takes Proofs converted to [[Field]] using proofToField and returns a choiceTree of all the permutations of that proof
getProofPermuts :: [[Field]] -> ChoiceTree ([[Field]], [Int])
getProofPermuts x = do p <- permutations (length x)
                       return (map (x !!) p, p)

-- takes a list of laws and returns a choiceTree of all the permutations of the list of laws
getLawPermuts :: [Law] -> ChoiceTree [Law]
getLawPermuts x = do p <- permutations (length x)
                     return (map (x !!) p)

-- Exercise helper to print question
genProof :: ([Field], [Int]) -> Exercise -> Exercise
genProof (quer, _solution) ex = ex{eQuestion=quer, eBroughtBy = ["Sanket S. Joshi", "Anmol Chachra"] }


-- Exercise helper to give feedback
genFeedback :: ([Field], [Int]) -> Map.Map String String -> ProblemResponse -> ProblemResponse
genFeedback (_, sol) mStrs resp = case Map.lookup "proof" mStrs of
                                    Just str
                                      -> if map ((sol !!) . read) (breakUnderscore str) == [0..((length sol)-1)]
                                         then markCorrect resp
                                         else markWrong resp{prFeedback=[FText "You answered: ",FText str]}
                                    Nothing -> error "Client response is missing 'proof' field"

-- Helper to break the solution received from the front end into a list
breakUnderscore :: String -> [String]
breakUnderscore s
  =  case dropWhile (=='_') s of
       "" -> []
       s' -> w : breakUnderscore s''
             where (w, s'') = break (=='_') s'
