module CS30.Exercises.ModN (modN) where
import CS30.Data
import CS30.Exercises.Data
import CS30.Exercises.Util
import qualified Data.Map as Map

type ModNEntry = (Field, Bool)


-- | Constant function, returns the generated exercise
modN :: ExerciseType
modN = exerciseType "ModN" "L25" "Modulo: True or False" 
              [exercise] genTable modNFeedback

-- | Generates the table of problems
genTable :: [(Field,bool)] -> Exercise -> Exercise
genTable myProblem def 
 = def{ eQuestion = [ FText $"Indicate for each answer whether it is true or false"
                    , FTable ([headerRow] ++                                                                                     -- add header row to the table
                                [ [Cell field, Cell (FFieldBool (FText "True") (FText "False") (Just False) ("TF_" ++ show i))]  -- add the body rows of the table
                                | ((field, _),i) <- myProblemIndexed ] )
                    ]
      , eBroughtBy = ["Paul Gralla","Joseph Hajjar"] }
 where 
       myProblemIndexed = zip myProblem ([1..]::[Int])
       headerRow = [Header (FText "Equation"),Header (FText "True/False") ]

-- | Constant which creates the ChoiceTree with all of the exercises
-- | Input: nothing
-- | Output: ChoiceTree with all of the exercises
exercise :: ChoiceTree [ModNEntry]
exercise
 = do base <- (2*)<$> nodes [2..10]
      perms <- permute [integerPowerSame base, addition base, multiplication base, integerPowerDiff base]
      sequenceA (take 3 perms)


-- | Generates a problem with two numbers (congruent under modulo) raised to the same power (always True)
-- | Input: modBase - the modular base of the equivalence
-- | Output: a ChoiceTree containing problems of this type with the given modBase
integerPowerSame :: Integer -> ChoiceTree ModNEntry
integerPowerSame modBase = Branch [ Branch [g leftNum expo modBase
                                           | expo <- [modBase..modBase*2] ] 
                                  | leftNum <- getLeftNums modBase]
    where
        g leftNum expo' mb = Node (FMath (show leftNum ++ "^{" ++ show expo' ++ "}" ++  "\\  \\equiv_{" ++ show mb ++ "} \\ \\ " ++ show rightNum ++ "^{" ++ show expo' ++ "}"), True)
            where rightNum = leftNum `mod` modBase

-- | Generates a problem with two numbers (congruent under modulo) raised to different powers (rarely True)
-- | Input: modBase - the modular base of the equivalence
-- | Output: a ChoiceTree containing problems of this type with the given modBase
integerPowerDiff :: Integer -> ChoiceTree ModNEntry
integerPowerDiff modBase = nodes [ (FMath (show leftNum ++ "^{" ++ show exponent1 ++ "}" ++  "\\  \\equiv_{" ++ show modBase ++ "} \\ \\ " ++ show leftNum ++ "^{" ++ show exponent2 ++ "}"), False)
                                   -- pick to different powers, by picking the same index from a list of even and a reversed list of odd numbers
                                 | leftNum <- [1..modBase-1]
                                 , exponent1 <- [modBase .. modBase*3]
                                 , let exponent2 = leftNum
                                 , modExp leftNum exponent1 modBase /= modExp leftNum exponent2 modBase
                                 ]

-- | Generates a problem where the same number is added to two numbers which are congruent under modulo (always True)
-- | Input: modBase - the modular base of the equivalence
-- | Output: a ChoiceTree containing problems of this type with the given modBase
addition :: Integer -> ChoiceTree ModNEntry 
addition modBase
  = Branch [ Branch [nodes [ (FMath (show leftNum ++ " + " ++ show summand ++ "\\  \\equiv_{" ++ show modBase ++ "} \\ \\ " ++ show rightNum ++ " + " ++ show summand), True)
                           , (FMath (show summand ++ " + " ++ show leftNum ++ "\\  \\equiv_{" ++ show modBase ++ "} \\ \\ " ++ show summand ++ " + " ++ show rightNum), True)
                           ]
                    | summand <- [1..modBase*2] ]
           | leftNum <- getLeftNums modBase
           , let rightNum = leftNum `mod` modBase]

-- | Generates a problem where the same number is multiplied to two numbers which are congruent under modulo (always True)
-- | Input: modBase - the modular base of the equivalence
-- | Output: a ChoiceTree containing problems of this type with the given modBase
multiplication :: Integer -> ChoiceTree ModNEntry
multiplication modBase
  = Branch [ Branch [ nodes [ (FMath (show leftNum ++ " \\cdot " ++ show factor ++ "\\  \\equiv_{" ++ show modBase ++ "} \\ \\  " ++ show rightNum ++ " \\cdot " ++ show factor), True)
                            , (FMath (show factor ++ " \\cdot " ++ show leftNum ++ "\\  \\equiv_{" ++ show modBase ++ "} \\ \\  " ++ show factor ++ " \\cdot " ++ show rightNum), True)
                            ]
                    | factor <- [1..modBase*2] ] 
           | leftNum <- getLeftNums modBase
           , let rightNum = leftNum `mod` modBase]

-- taken from https://byorgey.wordpress.com/2020/02/15/competitive-programming-in-haskell-modular-arithmetic-part-1/
modExp :: Integer -> Integer -> Integer -> Integer
modExp _ 0 _ = 1
modExp b e m
  | even e    = (r*r) `mod` m
  | otherwise = (b*r*r) `mod` m
  where
    r = modExp b (e `div` 2) m

-- | Generates a list of numbers which are not multiples of the modBase (used in most exercise generating functions)
-- | Input: modBase - the modular base of the equivalence
-- | Output: a list of numbers which are not multiples of the modBase
getLeftNums :: Integer -> [Integer]
getLeftNums modBase = (filter (\x -> x `mod` modBase /= 0) [(modBase+1)..modBase*5-1])

-- | Feedback function
-- | Generates a table with (given solutions) | (correct solutions) if anything is found incorrect
-- | Otherwise, congratulates
modNFeedback :: [ModNEntry] -> Map.Map String String -> ProblemResponse -> ProblemResponse
modNFeedback qAndSol usr defaultRsp
    = reTime $ if (isCorrect) 
               then correct{prFeedback=[FText "Correct, champ!"] }
               else wrong{prFeedback=[FText "Wrong.",table]}
    where
        table = FTable ( -- feedback table, displays the question, the given answer and the correct solution
                        [[Header (FText "Equation"),Header (FText "Your Answer"), Header (FText "Solution") ]] ++ 
                        [[Cell question, Cell (FText (charToBoolStr sol)), Cell (FText $ show answer)]  | ((question, answer), sol) <- qAndSolandAns ]
                        )

        qAndSolandAns = zip qAndSol answers  -- zipped list of questions with solution and the given answer
        solutions = [show b | (_,b) <- qAndSol] -- get the solution for each question
        answers = [ answerLookup i | i <- [1..5]::[Int]] -- lookup the given answer for each question
        allAnsWithSol = zip solutions answers  -- zipped list of given answers and solutions
        answerLookup i = case x of
                      Nothing -> ""
                      Just a -> a
                      where x = Map.lookup ("TF_"++ show i) usr 
        
        isCorrect = all (==True) [ givenAns == (charToBoolStr solu) | (givenAns,solu) <- allAnsWithSol]  -- do all given answers match the correct solution
        charToBoolStr c = if(c == "F") then "False" else (if(c == "T") then "True" else "")
        wrong = markWrong defaultRsp
        correct = markCorrect defaultRsp
