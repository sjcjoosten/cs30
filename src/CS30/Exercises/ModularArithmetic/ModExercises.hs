{-# LANGUAGE TemplateHaskell #-}

module CS30.Exercises.ModularArithmetic.ModExercises (ModEx, unwrap, mods) where
import CS30.Data
import CS30.Exercises.Data
import Data.Aeson.TH

type Answer = Int
type Modulus = String
type Questions = [Field]

-- ModEx is represented as a tuple of questions (a list of fields),
-- the modulus (used while displaying the question), and the answer (used while parsing)
data ModEx = ModEx (Questions, Modulus, Answer) deriving Show

$(deriveJSON defaultOptions ''ModEx)

-- Returns the tuple wrapped by the ModEx constructor
unwrap :: ModEx -> (Questions, Modulus, Answer)
unwrap (ModEx (questions, modulus, answer)) = (questions, modulus, answer)

_trivialExercises :: [ModEx]
_trivialExercises = 
    [
        ModEx (
            [FMath "17 \\equiv_{1}"], "1", 0
        ),
        
        ModEx (
            [FMath "23 \\equiv_{1}"], "1", 0
        ),
        
        ModEx (
            [FMath "37 \\equiv_{1}"], "1", 0
        ),
        
        ModEx (
            [FMath "81 \\equiv_{1}"], "1", 0
        ),
        
        ModEx (
            [FMath "16 \\equiv_{10}"], "10", 6
        ),
        
        ModEx (
            [FMath "24 \\equiv_{10}"], "10", 4
        ),
        
        ModEx (
            [FMath "33 \\equiv_{10}"], "10", 3
        ),
        
        ModEx (
            [FMath "49 \\equiv_{10}"], "10", 9
        ),
        
        ModEx (
            [FMath "48 \\equiv_{3}"], "3", 0
        ),
        
        ModEx (
            [FMath "44 \\equiv_{11}"], "11", 0
        ),
        
        ModEx (
            [FMath "21 \\equiv_{7}"], "7", 0
        ),
        
        ModEx (
            [FMath "63 \\equiv_{7}"], "7", 0
        ),
        
        ModEx (
            [FMath "24 \\equiv_{3}"], "3", 0
        )
    ]

easyExercises :: [ModEx]
easyExercises = 
    [   
        ModEx (
            [FMath "-30 \\equiv_{7}"], "7", 5
        ),

        ModEx (
            [FMath "-9 \\equiv_{13}"], "13", 4
        ),

        ModEx (
            [FMath "83 \\equiv_{7}"], "7", 6
        ),

        ModEx (
            [FMath "59 \\equiv_{17}"], "17", 8
        ),

        ModEx (
            [FMath "91 \\equiv_{23}"], "23", 22
        ),
     
        ModEx (
            [FMath "-137 \\equiv_{5}"], "5", 3
        ),

        ModEx (
            [FMath "-41 \\equiv_{13}"], "13", 11
        ),

        ModEx (
            [FMath "(-38 + 19) \\equiv_{5}"], "5", 1
        ),

        ModEx (
            [FMath "(-29 + 7) \\equiv_{3}"], "3", 2
        ),

        ModEx (
            [FMath "(-22 \\cdot 6) \\equiv_{17}"], "17", 4
        ),

        ModEx (
            [FMath "(-13 \\cdot 4) \\equiv_{19}"], "19", 5
        ),

        ModEx (
            [FMath "(25 \\cdot 13) \\equiv_{23}"], "23", 3
        )
    ]

mediumExercises :: [ModEx]
mediumExercises = 
    [
        ModEx (
            [FMath "3^{5} \\equiv_{13}"], "13", 9
        ),

        ModEx (
            [FMath "4^{7} \\equiv_{7}"], "7", 4
        ),
        
        ModEx (
            [FMath "6^{2} \\equiv_{11}"], "11", 3
        ),
        
        ModEx (
            [FMath "2^{5} \\equiv_{5}"], "5", 2
        ),
        
        ModEx (
            [FMath "5^{3} \\equiv_{7}"], "7", 6
        ),

        ModEx (
            [FMath "(7^{5} - 3^{5}) \\equiv_{5}"], "5", 1
        ),

        ModEx (
            [FMath "(3^{4} - 2^{3}) \\equiv_{23}"], "23", 4
        ),

        ModEx (
            [FMath "(9^{3} - 5^{2}) \\equiv_{17}"], "17", 7
        ),

        ModEx (
            [FMath"(8^{14} - 9^{12}) \\equiv_{13}"], "13", 11
        ),

        ModEx (
            [FMath "(2451 + 2)^{5} \\equiv_{3}"], "3", 2
        ),

        ModEx (
            [FMath "(33 + 12)^{5} \\equiv_{7}"], "7", 5
        ),

        ModEx (
            [FMath "(17 - 2)^{3} \\equiv_{11}"], "11", 9
        ),

        ModEx (
            [FMath "(375 - 320)^{2} \\equiv_{2}"], "2", 1
        )
    ]

hardExercises :: [ModEx]
hardExercises = 
    [
        ModEx (
            [FMath "-19^{5} \\equiv_{13}"], "13", 11
        ),
        
        ModEx (
            [FMath "-476^{5} - 9^{8} \\equiv_{7}"], "7", 3
        ),
        
        ModEx (
            [FMath "-43^{3} \\cdot 72^{4} \\equiv_{5}"], "5", 3
        ),
        
        ModEx (
            [FMath "(-28^{4} + 59^{6}) \\equiv_{17}"], "17", 8
        ),

        ModEx (
            [FMath "16 \\cdot 37^{-1} \\equiv_{11}"], "11", 4
        ),

        ModEx (
            [FMath "52 \\cdot 24^{-1} \\equiv_{5}"], "5", 1
        ),

        ModEx (
            [FMath "33 \\cdot 35^{-1} \\equiv_{8}"], "8", 3
        ),

        ModEx (
            [FMath "54 \\cdot 72^{-1} \\equiv_{23}"], "23", 2
        ),

        ModEx (
            [FMath "(273 + 7)^{3} \\cdot 43^{-1} \\equiv_{4}"], "4", 1
        ),

        ModEx (
            [FMath "-17^{2} \\cdot 67^{-1} \\equiv_{5}"], "5", 3
        ),

        ModEx (
            [FMath "(3 - 18)^{3} \\cdot 32^{-1} \\equiv_{7}"], "7", 5
        ),

        ModEx (
            [FMath "(3^{2} - 3^{5}) \\cdot 25^{-1} \\equiv_{11}"], "11", 2
        )
    ]

mods :: [ChoiceTree ModEx]
mods = [nodes easyExercises, nodes mediumExercises, nodes hardExercises]