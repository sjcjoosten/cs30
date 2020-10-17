-- | This module doesn't really have an exercise, it is just there to show how to draw tables.
--   The exported exercise 'stub' does only that: draw a table with three rows and two columns.
module CS30.Exercises.ModN (modN) where
import CS30.Data
import CS30.Exercises.Data


modN :: ExerciseType
modN = exerciseType "Modulo N" "Modulo N" "True or False" 
              [boolTree] genTable unknownFeedback
        where unknownFeedback _ _ pr = pr

genTable :: a -> Exercise -> Exercise
genTable _ def 
 = def{ eQuestion = [ FText $"Here is an example table"
                    , FTable [row1,row2,row3] ]
      , eBroughtBy = ["Sebastiaan Joosten"] }
 where row1 = [Header (FText "numbers"),Header (FText "letters") ]
       row2 = [Cell   (FText "1"      ),Cell   (FText "A") ]
       row3 = [Cell   (FFieldBool (FText "0") (FText "2") (Just True) "nr_response"),Cell   (FFieldBool (FText "B") (FText "D") Nothing "letter_response") ]


-- gen_congruent :: Exercise