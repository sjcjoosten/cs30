module CS30.Exercises.ModularArithmetic.ModExercises (mods) where
import CS30.Data
import CS30.Exercises.Data

mods :: [ChoiceTree ([Field], [[String]])]
mods = [ 
    nodes [
        (
            [FText "48 modulo 3"], [["0"]]
        )
    ],
    nodes [
        (
            [FText "-30 modulo 4"], [["2"]]
        )
    ],      
    nodes [
        (
            [FText "(7^5 - 3^5) modulo 5"], [["1"]]
        )
    ],  
    nodes [
        (
            [FText "-19^5 modulo 13"], [["11"]]
        )
    ]]