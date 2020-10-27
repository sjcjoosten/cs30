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

--function to calculate inv modulo
modInv :: Int -> Int ->  Int
modInv a m
  | 1 == g =  (mkPos i)
  | otherwise = 0
  where
    (i, _, g) = gcdExt a m
    mkPos x
      | x < 0 = x + m
      | otherwise = x

-- Extended Euclidean algorithm.
-- Given non-negative a and b, return x, y and g
-- such that ax + by = g, where g = gcd(a,b).
-- Note that x or y may be negative.
gcdExt :: Int -> Int -> (Int, Int, Int)
gcdExt a 0 = (1, 0, a)
gcdExt a b =
  let (q, r) = a `quotRem` b
      (s, t, g) = gcdExt b r
  in (t, s - q * t, g)

-- Returns the tuple wrapped by the ModEx constructor
unwrap :: ModEx -> (Questions, Modulus, Answer)
unwrap (ModEx (questions, modulus, answer)) = (questions, modulus, answer)

-- Generates range of values for easy problems
genInts :: [Int]
genInts = [1 .. 100]
genIntsMed :: [Int]
genIntsMed = [50 .. 300]
genIntsHard :: [Int]
genIntsHard = [300 .. 1000]

divisors :: Int -> [Int]
divisors n = [x | x <- [2..(n - 1)], n `rem` x == 0]

-- Used to create prime moduli
primes :: [Int]
primes = [x | x <- [2 .. ], null (divisors x)]

--trivial level set
trivialSets1,trivialSets2,trivialSets3 :: ChoiceTree (Questions, Modulus, Answer)
trivialSets1 = Branch [Node ([FMath (show x ++ "\\equiv_{ 1 }")], "1", 0) 
                      | x <- take 90 genInts]
trivialSets2 = Branch [Node ([FMath (show x ++ "\\equiv_{ 10 }")], "10", x `mod` 10) 
                      | x <- take 90 genInts]
trivialSets3 = Branch [Branch [Node ([FMath (show (x*y) ++ "\\equiv_{" ++ show y ++ "}")], show y, 0) 
                              | x <- take 90 genInts, y < x]
                      | y <- take 15 primes]

trivialExercises1,trivialExercises2,trivialExercises3 :: ChoiceTree (ModEx)
trivialExercises1 =  fmap (ModEx) trivialSets1
trivialExercises2 =  fmap (ModEx) trivialSets2
trivialExercises3 =  fmap (ModEx) trivialSets3

--easy level set
easySets1,easySets2,easySets3,easySets4 :: ChoiceTree (Questions, Modulus, Answer)
easySets1 = Branch[ Branch [Node ([FMath (show x ++ "\\equiv_{" ++ show y ++ "}")], show y, x `mod` y) 
                           | x <- take 90 genInts, y < x]
                  |y <- take 15 primes]

easySets2 = Branch[ Branch [Node ([FMath (show (-x) ++ "\\equiv_{" ++ show y ++ "}")], show y, (-x) `mod` y) 
                           | x <- take 90 genInts, y < x]
                  |y <- take 15 primes]

easySets3 = Branch[ Branch [ Branch [Node ([FMath (show x ++ "+" ++ show z ++ "\\equiv_{" ++ show y ++ "}")], show y, (x+z) `mod` y) 
                                    | x <- take 90 genInts, y < x] 
                           |z <- take 90 genInts] 
                  |y <- take 15 primes]

easySets4 = Branch[Branch [Branch [Node ([FMath (show (-x) ++ "*" ++ show z ++ "\\equiv_{" ++ show y ++ "}")], show y, (-x*z) `mod` y) 
                                  | x <- take 90 genInts, y < x]
                          |z <- take 90 genInts]
                  |y <- take 15 primes]

easyExercises1,easyExercises2,easyExercises3,easyExercises4 :: ChoiceTree (ModEx)
easyExercises1 =  fmap (ModEx) easySets1
easyExercises2 =  fmap (ModEx) easySets2
easyExercises3 =  fmap (ModEx) easySets3
easyExercises4 =  fmap (ModEx) easySets4

--medium level set
mediumSets1,mediumSets2,mediumSets3,mediumSets4 :: ChoiceTree (Questions, Modulus, Answer)
mediumSets1 = Branch [Branch[ Branch [Node ([FMath (show x ++ "^{" ++ show z ++ "} \\equiv_{" ++ show y ++ "}")], show y, (x^z) `mod` y) 
                                     |x <- take 200 genIntsMed, y < x]
                            |z <- take 90 genInts]
                      |y <- take 15 primes]

mediumSets2 = Branch [Branch [Branch [Branch[ Branch[ Node([FMath ("(" ++ show x ++ "^{" ++  show z ++ "} - " ++ show w ++ "^{" ++  show m ++ "}) \\equiv_{" ++ show y ++ "}")], show y, ((x^z) - (w^m)) `mod` y) 
                                                    | x <- take 200 genIntsMed, y < x]
                                            |z <- take 90 genInts]
                                      |m <- take 90 genInts] 
                              |w <- take 200 genIntsMed]
                     |y <- take 15 primes]

mediumSets3 = Branch [Branch [Branch[ Branch[ Node([FMath ("(" ++ show x ++ "+" ++  show w ++ ")^{" ++ show z ++ "} \\equiv_{" ++ show y ++ "}")], show y, ((x+w)^z) `mod` y) 
                                            | x <- take 200 genIntsMed, y < x]
                                    |z <- take 90 genInts] 
                             |w <- take 200 genIntsMed] 
                     |y <- take 15 primes]

mediumSets4 = Branch [Branch [Branch[ Branch[ Node([FMath ("(" ++ show x ++ "-" ++  show w ++ ")^{" ++ show z ++ "} \\equiv_{" ++ show y ++ "}")], show y, ((x-w)^z) `mod` y) 
                                            | x <- take 200 genIntsMed, y < x]
                                    | z <- take 90 genInts]
                             | w <- take 200 genIntsMed]
                     | y <- take 15 primes]

mediumExercises1,mediumExercises2,mediumExercises3,mediumExercises4 :: ChoiceTree (ModEx)
mediumExercises1 =  fmap (ModEx) mediumSets1
mediumExercises2 =  fmap (ModEx) mediumSets2
mediumExercises3 =  fmap (ModEx) mediumSets3
mediumExercises4 =  fmap (ModEx) mediumSets4

--hard level set
hardSets1,hardSets2,hardSets3,hardSets4 :: ChoiceTree (Questions, Modulus, Answer)
hardSets1 = Branch [ Branch[ Branch [Node([FMath (show (-x) ++ "^{" ++ show z ++ "} \\equiv_{" ++ show y ++ "}")], show y, ((-x)^z) `mod` y) 
                                    | x <- take 600 genIntsHard, y < x]
                           | y <- take 90 primes]
                   | z <- take 200 genIntsMed]

hardSets2 = Branch [ Branch[ Branch [Node ([FMath (show x ++ "\\cdot" ++ show z ++ "^{-1} \\equiv_{" ++ show y ++ "}")], show y, (((x `mod` y)*(z `modInv` y)) `mod` y)) 
                                    | x <- take 600 genIntsHard, y < x]
                            |y <- take 90 primes]
                   |z <- take 200 genIntsMed]

hardSets3 = Branch [Branch [Branch[ Branch[ Branch [Node ([FMath ("(" ++ show x ++ "+" ++  show w ++ ")^{" ++ show z ++ "} \\cdot" ++ show l ++ "^{-1} \\equiv_{" ++ show y ++ "}")], show y,mod((((x+w)^z) `mod` y)*(l `modInv` y)) y) 
                                                   | x <- take 600 genIntsHard, y < x] 
                                          |z <- take 90 genInts]
                                  |w <- take 200 genIntsMed]
                           |l <- take 90 primes]
                   |y <- take 90 primes]

hardSets4 = Branch [ Branch [ Branch [ Branch [ Branch[ Branch [Node ([FMath ("(" ++ show x ++ "^{" ++  show z ++ "} - " ++ show w ++ "^{" ++  show m ++ "}) \\cdot" ++ show l ++ "^{-1} \\equiv_{" ++ show y ++ "}")], show y, mod(((x^z - w^m) `mod` y)*(l `modInv` y)) y)
                                                               | x <- take 600 genIntsHard, y < x]
                                                      |z <- take 200 genIntsMed]
                                              |m <- take 90 genInts]
                                      |w <- take 200 genIntsMed]
                             |l <- take 90 primes]
                   |y <- take 90 primes]

hardExercises1,hardExercises2,hardExercises3,hardExercises4 :: ChoiceTree (ModEx)
hardExercises1 = fmap (ModEx) hardSets1
hardExercises2 = fmap (ModEx) hardSets2
hardExercises3 = fmap (ModEx) hardSets3
hardExercises4 = fmap (ModEx) hardSets4

mods :: [ChoiceTree (ModEx)]
mods =  [Branch [trivialExercises1              
                ,trivialExercises2
                ,trivialExercises3]
        ,Branch [easyExercises1
                ,easyExercises2
                ,easyExercises3
                ,easyExercises4]
        ,Branch [mediumExercises1
                ,mediumExercises2
                ,mediumExercises3
                ,mediumExercises4]
        ,Branch [hardExercises1
                ,hardExercises2
                ,hardExercises3
                ,hardExercises4]
        ]