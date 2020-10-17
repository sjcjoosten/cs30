-- | This module doesn't really have an exercise, it is just there to show how to draw tables.
--   The exported exercise 'stub' does only that: draw a table with three rows and two columns.
module CS30.Exercises.ModN (modN) where
import CS30.Data
import CS30.Exercises.Data
import qualified Data.Map as Map


modN :: ExerciseType
modN = exerciseType "Modulo N" "Modulo N" "True or False" 
              [boolTree] genTable modNFeedback
        -- where unknownFeedback _ _ pr = pr

genTable :: a -> Exercise -> Exercise
genTable _ def 
 = def{ eQuestion = [ FText $"Yo answer those questions"
                    , FTable [row1,row2,row3,row4,row5] ]
      , eBroughtBy = ["Paul Gralla and Joseph Hajjar"] }
 where row1 = [Header (FText "Equation"),Header (FText "True/False") ]
       row2 = [Cell   (FText "Example 1" ), Cell   (FFieldBool (FText "True") (FText "False") (Just False) "TF_1") ]
       row3 = [Cell   (FText "Example 2" ), Cell   (FFieldBool (FText "True") (FText "False") (Just False) "TF_2") ]
       row4 = [Cell   (FText "Example 3" ), Cell   (FFieldBool (FText "True") (FText "False") (Just False) "TF_3") ]
       row5 = [Cell   (FText "Example 4" ), Cell   (FFieldBool (FText "True") (FText "False") (Just False) "TF_4") ]

    --    row3 = [Cell   (FFieldBool (FText "0") (FText "2") (Just True) "nr_response"),Cell   (FFieldBool (FText "B") (FText "D") Nothing "letter_response") ]


-- Inputs for these TBD, maybe random seed?

-- generates a congruent addition example
gen_addition :: Int -> String  -- always congruent
gen_addition = ""

gen_multiplication :: String  -- always congruent
gen_multiplication = ""

gen_intPower :: String -- 
gen_intPower = ""

gen_fracPower :: String
gen_fracPower = ""

-- right parameters??
modNFeedback :: Bool -> Map.Map String String -> ProblemResponse -> ProblemResponse
modNFeedback a b defaultRsp = wrong{prFeedback=[(FText "wrong you mongoose")]}


    where
        wrong = markWrong defaultRsp
        correct = markCorrect defaultRsp