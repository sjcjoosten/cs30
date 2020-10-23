module CS30.Exercises.ProbWord.Basics (basicprob) where
import           CS30.Data
import           CS30.Exercises.Data
import GHC.Real -- for (%)

-- sumList :: [Integer] -> Integer


basicprob :: [ChoiceTree ([Field], Rational)]
basicprob = [ Branch [ nodes [ ( [FText $ "there are " ++ show blue ++ " blue marbles, " ++ show red ++ " red marbles, and " ++ show green ++ " green marbles, what is the probability of picking a blue marble?", FFieldMath "prob"]
                                , blue % (blue + red + green)
                                )
                              , ( [FText $ "there are " ++ show blue ++ " blue marbles, " ++ show red ++ " red marbles, and " ++ show green ++ " green marbles, what is the probability of picking a marble that isn't blue?", FFieldMath "prob"]
                                , (red + green) % (blue + red + green)
                                )
                              ]
                              | blue <- [1..8], red <- [1..8], green <- [1..8] ]
            , Branch [ nodes [ ( [FText $ show numDice ++ " fair dice are rolled, find the probability that the sum of the results is equal to " ++ show sumDice, FFieldMath "prob"]
                                , (toInteger $ nDiceEqualToK numDice sumDice) % (6 ^ numDice)
                               )
                              ]
                              | numDice <- [2..3], sumDice <- [numDice + 1..numDice*6 - 1]
                              ]
            , Branch [ nodes [([FText $ show numDice ++ " fair dice are rolled, find the probability that the sum of the results is less than " ++ show sumDice, FFieldMath "prob"]
                              , (toInteger $ nDiceLessThanK numDice sumDice) % (6 ^ numDice))                              
                              | numDice <- [2..3], sumDice <- [2*numDice..numDice*5]] -- to prevent degenerate question, only take [2*numDice, 5*numDice] as question domain -}
                        ]
              ]

-- Throw N dice, how many outcomes of a sum that is less than K
nDiceLessThanK :: Int -> Int -> Int
nDiceLessThanK dice_n k
  | k < dice_n = 0
  | k > dice_n * 6 = 6 ^ dice_n
  | otherwise = lessThanCount dice_n !! (k - dice_n)

-- lessThanCount returns a list consisting of the count of getting a num less than some number
-- the first value is for how less than the sum of dice_n, that's always a zero  
-- lessThanCount 1 = [0,1,2,3,4,5,6]
-- lessThanCount 2 = [0,1,3,6,10,15,21,26,30,33,35,36]
lessThanCount :: Int -> [Int]
lessThanCount = scanl (\acc (_, o) -> o + acc) 0 . sumCount

-- Throw N dice, how many outcomes are of sum K?
nDiceEqualToK :: Int -> Int -> Int
nDiceEqualToK dice_n k =
  case lookup k (sumCount dice_n) of
    Nothing -> 0
    Just v -> v

-- sumCount of each of the possible sum
-- sumCount 1 = [(1,1),(2,1),(3,1),(4,1),(5,1),(6,1)]
-- sumCount 2 = [(2,1),(3,2),(4,3),(5,4),(6,5),(7,6),(8,5),(9,4),(10,3),(11,2),(12,1)]
sumCount:: Int -> [(Int, Int)]
sumCount dice_n = countOccurrence $ map sum $ cp $ take dice_n $ repeat [1 .. 6]

-- get the count of each element in the list
-- countOccurrence [3,1,4,1,5] = [(3,1),(4,1),(1,2),(5,1)]
countOccurrence :: [Int] -> [(Int, Int)]
countOccurrence [] = []
countOccurrence (x : xs') =
  let rest = countOccurrence xs'
      (prev, ys) = break (\(s, _) -> s == x) rest
   in case ys of
        [] -> ((x, 1) : rest)
        ((s, num) : ys') -> prev ++ ((s, num + 1) : ys')

-- Cartesian Product
cp :: [[a]] -> [[a]]
cp alist = case alist of [] -> [[]]; (xs : xss) -> [(x : ys) | x <- xs, ys <- cp xss]