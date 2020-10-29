module CS30.Exercises.ModN (modN) where
import CS30.Data
import CS30.Exercises.Data
import CS30.Exercises.Util
import Data.List
import qualified Data.Map as Map

type ModNEntry = (Field, Bool)


-- | Constant function, returns the generated exercise
modN :: ExerciseType
modN = exerciseType "ModNTF" "Modulo N" "True or False" 
              [exercise] genTable modNFeedback

-- | Generates the table of problems
genTable :: [(Field,bool)] -> Exercise -> Exercise
genTable myProblem def 
 = def{ eQuestion = [ FText $"Please answer those questions"
                    , FTable ([headerRow] ++                                                                                     -- add header row to the table
                                [ [Cell field, Cell (FFieldBool (FText "True") (FText "False") (Just False) ("TF_" ++ show i))]  -- add the body rows of the table
                                    | ((field, _),i) <- myProblemIndexed ] )
                    ]
      , eBroughtBy = ["Paul Gralla","Joseph Hajjar"] }
 where 
       myProblemIndexed = zip myProblem ([1..]::[Int])
       headerRow = [Header (FText "Equation"),Header (FText "True/False") ]


cartProduct :: [ChoiceTree a] -> ChoiceTree [a]
cartProduct [] = Node []
cartProduct (x:xs) = replace f (cartProduct xs)
    where
        f xs' = fmap (:xs') x

-- | Constant which creates the ChoiceTree with all of the exercises
-- | Input: nothing
-- | Output: ChoiceTree with all of the exercises
exercise :: ChoiceTree [ModNEntry]
exercise = replace cartProduct (Branch [Node [integerPowerSame modBase, addition modBase, multiplication modBase, integerPowerDiff modBase, fractionPower modBase]| modBase <- [4,6..20]])


-- | Generates a problem with two numbers (congruent under modulo) raised to the same power (always True)
-- | Input: modBase - the modular base of the equivalence
-- | Output: a ChoiceTree containing problems of this type with the given modBase
integerPowerSame :: Integer -> ChoiceTree ModNEntry
integerPowerSame modBase = Branch [ Branch [g leftNum expo modBase
                                            | expo <- [modBase..modBase*2] ] 
                                                          | leftNum <- getLeftNums modBase]
    where
        g leftNum expo' mb = Node (FMath (show leftNum ++ "^{" ++ show expo' ++ "}" ++  "\\  \\equiv_{" ++ show mb ++ "} \\ \\ " ++ show rightNum ++ "^{" ++ show expo' ++ "}"), True)
            where
                rightNum = leftNum `mod` modBase

-- | Generates a problem with two numbers (congruent under modulo) raised to different powers (rarely True)
-- | Input: modBase - the modular base of the equivalence
-- | Output: a ChoiceTree containing problems of this type with the given modBase
integerPowerDiff :: Integer -> ChoiceTree ModNEntry
integerPowerDiff modBase = Branch [ Branch [g leftNum exponent1 exponent2 modBase
                                           | (exponent1, exponent2) <- (zip [modBase, modBase + 2..modBase*2] (reverse [modBase - 1, modBase + 1..modBase*2])) ]  -- pick to different powers, by picking the same index from a list of even and a reversed list of odd numbers
                                  | leftNum <- getLeftNums modBase]
    where
        g leftNum exponent1 exponent2 modBase' = Node (FMath (show leftNum ++ "^{" ++ show exponent1 ++ "}" ++  "\\  \\equiv_{" ++ show modBase' ++ "} \\ \\ " ++ show rightNum ++ "^{" ++ show exponent2 ++ "}"), isCorrect)
            where
                rightNum = leftNum `mod` modBase
                isCorrect = modExp leftNum exponent1 modBase == modExp rightNum exponent2 modBase

-- | Generates a problem with two numbers (congruent under modulo) square rooted (rarely True)
-- | Input: modBase - the modular base of the equivalence
-- | Output: a ChoiceTree containing problems of this type with the given modBase
fractionPower :: Integer -> ChoiceTree ModNEntry
fractionPower modBase = Branch [ g leftNum rightNum modBase
                               | rightNum <- squares
                               , leftNum <- (filter (\x -> (x `mod` modBase == rightNum `mod` modBase) && x /=rightNum ) squares)
                               -- Pick left and right number out of list of squares such that they have that they are congruent before the sqrt operation
                               ]
    where
        g leftNum rightNum modBase' = Node (FMath ("\\sqrt{" ++ show leftNum ++ "}" ++  "\\  \\equiv_{" ++ show modBase' ++ "} \\ \\ \\sqrt{" ++ show rightNum ++ "}"), isCorrect)
            where
                isCorrect = (rootOfPerfectSquare leftNum) `mod` modBase == (rootOfPerfectSquare rightNum) `mod` modBase  

-- | Generates a problem where the same number is added to two numbers which are congruent under modulo (always True)
-- | Input: modBase - the modular base of the equivalence
-- | Output: a ChoiceTree containing problems of this type with the given modBase
addition :: Integer -> ChoiceTree ModNEntry 
addition modBase = Branch [ Branch [g leftNum summand modBase 
                                    | summand <- [1..modBase*2] ] 
                                    | leftNum <- getLeftNums modBase]
    where
        g leftNum summand modBase' = Node (FMath (show leftNum ++ " + " ++ show summand ++ "\\  \\equiv_{" ++ show modBase' ++ "} \\ \\ " ++ show rightNum ++ " + " ++ show summand), True)
            where
                rightNum = leftNum `mod` modBase

-- | Generates a problem where the same number is multiplied to two numbers which are congruent under modulo (always True)
-- | Input: modBase - the modular base of the equivalence
-- | Output: a ChoiceTree containing problems of this type with the given modBase
multiplication :: Integer -> ChoiceTree ModNEntry
multiplication modBase = Branch [ Branch [g leftNum factor modBase
                                         | factor <- [1..modBase*2] ] 
                                | leftNum <- getLeftNums modBase]
    where
        g leftNum factor modBase' = Node (FMath (show leftNum ++ " \\cdot " ++ show factor ++ "\\  \\equiv_{" ++ show modBase' ++ "} \\ \\  " ++ show rightNum ++ " \\cdot " ++ show factor), True)
            where
                rightNum = leftNum `mod` modBase


-- | List of squares in [1,400] from which to choose perfect squares for fractionPower
squares :: [Integer]
squares = [x*x | x <- [1..20]]

-- | General square root function (given the input is a perfect square)
-- | Input: sq - a perfect square in [1,400]
-- | Output: index (the square root) if square root is found else -1
rootOfPerfectSquare :: Integer -> Integer
rootOfPerfectSquare sq = 
    case index of
        Just i -> toInteger i
        Nothing -> -1
        where index = elemIndex sq [x*x | x <- [1..20]]

-- taken from https://gist.github.com/trevordixon/6788535
modExp :: Integer -> Integer -> Integer -> Integer
modExp x y n = mod (x^(mod y (n-1))) n

-- | Generates a list of numbers which are not multiples of the modBase (used in most exercise generating functions)
-- | Input: modBase - the modular base of the equivalence
-- | Output: a list of numbers which are not multiples of the modBase
getLeftNums :: Integer -> [Integer]
getLeftNums modBase = (filter (\x -> x `mod` modBase /= 0)[(modBase+1)..modBase*5])

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
