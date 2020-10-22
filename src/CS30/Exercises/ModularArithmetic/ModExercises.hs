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

easyExercises :: [ModEx]
easyExercises = 
    [
        ModEx (
            [FMath "48 \\equiv_{3}"], "3", 0
        ),
        
        ModEx (
            [FMath "-30 \\equiv_{4}"], "4", 2
        ),

        ModEx (
            [FMath "-9 \\equiv_{16}"], "16", 7
        ),

        ModEx (
            [FMath "83 \\equiv_{7}"], "7", 6
        )
    ]

mediumExercises :: [ModEx]
mediumExercises = 
    [
        ModEx (
            [FMath "(7^{5} - 3^{5}) \\equiv_{5}"], "5", 1
        ),

        ModEx (
            [FMath "16 \\cdot 37^{-1} \\equiv_{11}"], "11", 4
        )
    ]

hardExercises :: [ModEx]
hardExercises = 
    [
        ModEx (
            [FMath "-19^{5} \\equiv_{13}"], "13", 11
        ),

        ModEx (
            [FMath "(245176 + 2)^{5} \\equiv_{6}"], "6", 2
        )
    ]

mods :: [ChoiceTree ModEx]
mods = [nodes easyExercises, nodes mediumExercises, nodes hardExercises]
