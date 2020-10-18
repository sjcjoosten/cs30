-- | This module doesn't really have an exercise, it is just there to show how to draw tables.
--   The exported exercise 'stub' does only that: draw a table with three rows and two columns.
module CS30.Exercises.ModN (modN) where
import CS30.Data
import CS30.Exercises.Data
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.Random (randomRIO)
import qualified Data.Map as Map


modN :: ExerciseType
modN = exerciseType "Modulo N" "Modulo N" "True or False" 
              [easyExercise] genTable modNFeedback
        -- where unknownFeedback _ _ pr = pr

genTable :: [(Field,bool)] -> Exercise -> Exercise
genTable myProblem def 
 = def{ eQuestion = [ FText $"Please answer those questions"
                    , FTable ([headerRow] ++ 
                                [ [Cell field, Cell (FFieldBool (FText "True") (FText "False") (Just False) ("TF_" ++ show i))]
                                | ((field, _),i) <- myProblemIndexed ] )
                    ]
      , eBroughtBy = ["Paul Gralla","Joseph Hajjar"] }
 where 
       myProblemIndexed = zip myProblem [1..]
       headerRow = [Header (FText "Equation"),Header (FText "True/False") ]
    

    --    row3 = [Cell   (FFieldBool (FText "0") (FText "2") (Just True) "nr_response"),Cell   (FFieldBool (FText "B") (FText "D") Nothing "letter_response") ]

easyExercise = Branch [ replace (f modBase) (easyMultiplication modBase) | modBase <- [4,6..20] ]
    where
        f modBase exercise = replace (g exercise) (easyAddition modBase)
        g e1 e2 = Node (e1 ++ e2)


easyAddition modBase = Branch [ Branch [g leftNum summand modBase| summand <- [1..modBase*2] ] | leftNum <- [modBase..modBase*5]] 
    where
        g leftNum summand modBase = Node [(FMath (show leftNum ++ " + " ++ show summand ++ "\\  \\equiv_{" ++ show modBase ++ "} \\ " ++ show rightNum ++ " + " ++ show summand), True)]
            where
                rightNum = leftNum `mod` modBase


easyMultiplication modBase = Branch [ Branch [g leftNum factor modBase| factor <- [1..modBase*2] ] | leftNum <- [modBase..modBase*5]]
    where
        g leftNum factor modBase = Node [(FMath (show leftNum ++ " \\cdot " ++ show factor ++ "\\  \\equiv_{" ++ show modBase ++ "} \\ " ++ show rightNum ++ " \\cdot " ++ show factor), True)]
            where
                rightNum = leftNum `mod` modBase

-- genProblemBaseN :: Integer -> ChoiceTree [(Field, Bool)]
-- genProblemBaseN modBase = Branch (map g [modBase..modBase*5])
--     where
--         g leftNum summand = Node [(FMath (show leftNum ++ " + " ++ show summand ++ "\\  \\equiv_{" ++ show modBase ++ "} \\ " ++ show rightNum ++ " + " ++ show summand), True)]
--          where
--             rightNum = leftNum `mod` modBase
--             summand = Branch [1..modBase*5]

-- Inputs for these TBD, maybe random seed?

-- generates a congruent addition example
-- gen_addition :: Int -> String  -- always congruent
-- gen_addition modBase = show leftNum ++ " + " ++ show summand ++ "\\eqiv_{" ++ show modBase ++ "} " ++ show rightNum ++ " + " ++ show summand
--     where
--         leftNum = read (show (liftIO$ randomRIO(modBase,modBase*5))) :: Int
--         rightNum = leftNum `mod` modBase
--         summand = read (show (liftIO$ randomRIO(1,modBase*2))) :: Int


gen_multiplication :: String  -- always congruent
gen_multiplication = ""

gen_intPower :: String -- 
gen_intPower = ""

gen_fracPower :: String
gen_fracPower = ""

-- right parameters??
modNFeedback :: [(Field, Bool)] -> Map.Map String String -> ProblemResponse -> ProblemResponse
modNFeedback a b defaultRsp = wrong{prFeedback=[(FText "wrong")]}


    where
        wrong = markWrong defaultRsp
        correct = markCorrect defaultRsp