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

-- Generates range of values for easy problems
easyGenInts :: [Int]
easyGenInts = [10 .. 100]
hardGenInts :: [Int]
hardGenInts = [100 .. 1000]

divisors :: Int -> [Int]
divisors n = [x | x <- [2..(n - 1)], n `rem` x == 0]

-- Used to create prime moduli
primes :: [Int]
primes = [x | x <- [2 .. ], null (divisors x)]

trivialSets1,trivialSets2,trivialSets3 :: [(Questions, Modulus, Answer)]
trivialSets1 = [([FMath (show x ++ "\\equiv_{ 1 }")], "1", 0) | x <- easyGenInts]
trivialSets2 = [([FMath (show x ++ "\\equiv_{ 10 }")], "10", x `mod` 10) | x <- easyGenInts]
trivialSets3 = [([FMath (show (x*y) ++ "\\equiv_{" ++ show y ++ "}")], show y, 0) | x <- easyGenInts, y <- take 15 primes, y < x]

trivialExercises1,trivialExercises2,trivialExercises3 ::  [ModEx]
trivialExercises1 = take 500 $ drop 500 $ map (ModEx) trivialSets1
trivialExercises2 = take 500 $ drop 500 $ map (ModEx) trivialSets2
trivialExercises3 = take 500 $ drop 500 $ map (ModEx) trivialSets3

easySets1,easySets2,easySets3,easySets4 :: [(Questions, Modulus, Answer)]
easySets1 = [([FMath (show x ++ "\\equiv_{" ++ show y ++ "}")], show y, x `mod` y) | x <- easyGenInts, y <- take 15 primes, y < x]
easySets2 = [([FMath (show (-x) ++ "\\equiv_{" ++ show y ++ "}")], show y, (-x) `mod` y) | x <- easyGenInts, y <- take 15 primes, y < x]
easySets3 = [([FMath (show (x+z) ++ "\\equiv_{" ++ show y ++ "}")], show y, (x+z) `mod` y) | x <- easyGenInts, z <- easyGenInts, y <- take 15 primes, y < x]
easySets4 = [([FMath (show ((-x)*z) ++ "\\equiv_{" ++ show y ++ "}")], show y, (-x*z) `mod` y) | x <- easyGenInts, z <- easyGenInts, y <- take 15 primes, y < x]

easyExercises1,easyExercises2,easyExercises3,easyExercises4 ::  [ModEx]
easyExercises1 = take 500 $ drop 500 $ map (ModEx) easySets1
easyExercises2 = take 500 $ drop 500 $ map (ModEx) easySets2
easyExercises3 = take 500 $ drop 500 $ map (ModEx) easySets3
easyExercises4 = take 500 $ drop 500 $ map (ModEx) easySets4

mediumSets1,mediumSets2,mediumSets3,mediumSets4 :: [(Questions, Modulus, Answer)]
mediumSets1 = [([FMath (show x ++ "^{" ++ show z ++ "} \\equiv_{" ++ show y ++ "}")], show y, (x^z) `mod` y) | x <- hardGenInts, z <- easyGenInts, y <- take 15 primes, y < x]
mediumSets2 = [([FMath ("(" ++ show x ++ "^{" ++  show z ++ "} - " ++ show w ++ "^{" ++  show z ++ "}) \\equiv_{" ++ show y ++ "}")], show y, (x^z - w^z) `mod` y) | x <- hardGenInts, z <- easyGenInts, w <- easyGenInts, y <- take 15 primes, y < x]
mediumSets3 = [([FMath ("(" ++ show x ++ "+" ++  show w ++ ")^{" ++ show z ++ "} \\equiv_{" ++ show y ++ "}")], show y, (x+w)^z `mod` y) | x <- hardGenInts, z <- easyGenInts, w <- easyGenInts, y <- take 15 primes, y < x]
mediumSets4 = [([FMath ("(" ++ show x ++ "-" ++  show w ++ ")^{" ++ show z ++ "} \\equiv_{" ++ show y ++ "}")], show y, (x-w)^z `mod` y) | x <- hardGenInts, z <- easyGenInts, w <- easyGenInts, y <- take 15 primes, y < x]

mediumExercises1,mediumExercises2,mediumExercises3,mediumExercises4 ::  [ModEx]
mediumExercises1 = take 500 $ drop 500 $ map (ModEx) mediumSets1
mediumExercises2 = take 500 $ drop 500 $ map (ModEx) mediumSets2
mediumExercises3 = take 500 $ drop 500 $ map (ModEx) mediumSets3
mediumExercises4 = take 500 $ drop 500 $ map (ModEx) mediumSets4

hardSets1,hardSets2,hardSets3,hardSets4 :: [(Questions, Modulus, Answer)]
hardSets1 = [([FMath (show (-x) ++ "^{" ++ show z ++ "} \\equiv_{" ++ show y ++ "}")], show y, ((-x)^z) `mod` y) | x <- hardGenInts, z <- easyGenInts, y <- take 15 primes, y < x]
hardSets2 = [([FMath (show x ++ "\\cdot" ++ show z ++ "^{-1} \\equiv_{" ++ show y ++ "}")], show y, (x*(z^(-1))) `mod` y) | x <- hardGenInts, z <- easyGenInts, y <- take 15 primes, y < x]
hardSets3 = [([FMath ("(" ++ show x ++ "+" ++  show w ++ ")^{" ++ show z ++ "} \\cdot" ++ show l ++ "^{-1} \\equiv_{" ++ show y ++ "}")], show y, (((x+w)^z)*(l^(-1))) `mod` y) | x <- hardGenInts, z <- easyGenInts,w <- easyGenInts,l <- easyGenInts, y <- take 15 primes, y < x]
hardSets4 = [([FMath ("(" ++ show x ++ "^{" ++  show z ++ "} - " ++ show w ++ "^{" ++  show z ++ "} \\cdot" ++ show l ++ "^{-1} \\equiv_{" ++ show y ++ "}")], show y, ((x^z - w^z)*(l^(-1))) `mod` y) | x <- hardGenInts, z <- easyGenInts,w <- easyGenInts,l <- easyGenInts, y <- take 15 primes, y < x]

hardExercises1,hardExercises2,hardExercises3,hardExercises4 ::  [ModEx]
hardExercises1 = take 500 $ drop 500 $ map (ModEx) hardSets1
hardExercises2 = take 500 $ drop 500 $ map (ModEx) hardSets2
hardExercises3 = take 500 $ drop 500 $ map (ModEx) hardSets3
hardExercises4 = take 500 $ drop 500 $ map (ModEx) hardSets4

mods :: [ChoiceTree (ModEx)]
mods = [ Branch [ nodes trivialExercises1
                , nodes trivialExercises2
                , nodes trivialExercises3]
        ,Branch [Branch [ nodes easyExercises1],
                 Branch [ nodes easyExercises2],
                 Branch [ nodes easyExercises3],
                 Branch [ nodes easyExercises4]]
        ,Branch [Branch [ Branch [nodes mediumExercises1]],
                 Branch [ Branch [nodes mediumExercises2]],
                 Branch [ Branch [nodes mediumExercises3]],
                 Branch [ Branch [nodes mediumExercises4]]]
        ,Branch [Branch [ Branch [Branch [nodes hardExercises1]]],
                 Branch [ Branch [Branch [nodes hardExercises2]]],
                 Branch [ Branch [Branch [nodes hardExercises3]]],
                 Branch [ Branch [Branch [nodes hardExercises4]]]]
        ]