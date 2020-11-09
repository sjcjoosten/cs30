{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.ModuloGenerateEx where

import Data.List
import qualified Data.Map as Map

import CS30.Data
import CS30.Exercises.Data
import CS30.Exercises.SetBasics.SolutionChecker

import CS30.Exercises.ModuloProof
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
modProofs = [nodes (concat [map (\(x,y) -> ([FText $"Can you put it in the right order?",
                                             FIndented 1 [FMath (show exp)], FReorder "proof" x], y))
                            (prPermuts exp) | exp <- gen_expressions [charAdd,
                                                                      charMul,
                                                                      charSub,
                                                                      charPow]]) -- level 1
              -- [ | exp <- gen_expressions (charAdd, charMul, charSub)],
              -- [ | exp <- gen_expressions (charAdd, charMul, charSub, charPow)]
            ]
            where
              parsing x = parseExpression True x
              derive y  = getDerivation 5 arithmetic_laws (parsing y)
              prPermuts e = getProofPermuts (proofToField (derive e))

getProofPermuts :: [[Field]] -> [([[Field]], [Int])]
getProofPermuts [] = []
getProofPermuts x = [(map (x !!) p, p) | p <- permutations [0..((length x)-1)]]

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

proofToField :: Proof -> [[Field]]
proofToField (Proof exp steps) = [ wrapField st | st <- steps]
  where
    wrapField (a,b) = [FMath "=", FText a, FIndented 1 [FMath (show b)]]

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