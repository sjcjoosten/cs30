-- | This module doesn't really have an exercise, it is just there to show how to draw tables.
--   The exported exercise 'stub' does only that: draw a table with three rows and two columns.
module CS30.Exercises.ModN (modN) where
import CS30.Data
import CS30.Exercises.Data
import Data.Functor.Identity
import CS30.Exercises.Util
import Data.List
import Control.Monad.IO.Class (MonadIO(liftIO))
import System.Random (randomRIO)
import qualified Data.Map as Map


modN :: ExerciseType
modN = exerciseType "Modulo N" "Modulo N" "True or False" 
              [easyExercise] genTable modNFeedback
        -- where unknownFeedback _ _ pr = pr

genTable :: [(Field,bool)] -> Exercise -> Exercise
genTable myProblem def 
 = def{ eQuestion = [ FText $"Please answer those questions"
                    , FTable ([headerRow] ++ 
                                [ [Cell field, Cell (FFieldBool (FText "True") (FText "False") (Just False) ("TF_" ++ show i))]
                                    | ((field, _),i) <- myProblemIndexed ] )
                    ]
      , eBroughtBy = ["Paul Gralla","Joseph Hajjar"] }
 where 
       myProblemIndexed = zip myProblem [1..]
       headerRow = [Header (FText "Equation"),Header (FText "True/False") ]
    

cartProduct :: [ChoiceTree a] -> ChoiceTree [a]
cartProduct [] = Node []
cartProduct (x:xs) = replace f (cartProduct xs)
    where
        f xs' = fmap (:xs') x

    --    row3 = [Cell   (FFieldBool (FText "0") (FText "2") (Just True) "nr_response"),Cell   (FFieldBool (FText "B") (FText "D") Nothing "letter_response") ]

-- easyExercise = cartProduct [easyAddition 4, easyMultiplication 4]
    

-- easyExercise = cartProduct [easyAddition 4, easyMultiplication 4]
easyExercise :: ChoiceTree [(Field, Bool)]
easyExercise = cartProduct [ [easyIntegerPowerSame modBase, easyAddition modBase, easyIntegerPowerSame modBase, easyIntegerPowerDiff modBase]| modBase <- [4,6..20]]

-- easyExercise = Branch [ replace (f modBase) (easyFractionPower modBase) | modBase <- [4,6..20] ]
--     where
--         f modBase exercise = replace (g exercise) (easyAddition modBase)
--         g e1 e2 = Node (e1 ++ e2)

easyIntegerPowerSame :: Integer -> ChoiceTree [(Field, Bool)]
easyIntegerPowerSame modBase = Branch [ Branch [g leftNum exponent modBase| exponent <- [modBase..modBase*2] ] | leftNum <- getLeftNums modBase]
    where
        g leftNum exponent modBase = Node [(FMath (show leftNum ++ "^{" ++ show exponent ++ "}" ++  "\\  \\equiv_{" ++ show modBase ++ "} \\ \\ " ++ show rightNum ++ "^{" ++ show exponent ++ "}"), True)]
            where
                rightNum = leftNum `mod` modBase

easyIntegerPowerDiff :: Integer -> ChoiceTree [(Field, Bool)]
easyIntegerPowerDiff modBase = Branch [ Branch [g leftNum exponent1 exponent2 modBase| (exponent1, exponent2) <- (zip [modBase, modBase + 2..modBase*2] (reverse [modBase - 1, modBase + 1..modBase*2])) ] | leftNum <- getLeftNums modBase]
    where
        g leftNum exponent1 exponent2 modBase = Node [(FMath (show leftNum ++ "^{" ++ show exponent1 ++ "}" ++  "\\  \\equiv_{" ++ show modBase ++ "} \\ \\ " ++ show rightNum ++ "^{" ++ show exponent2 ++ "}"), isCorrect)]
            where
                rightNum = leftNum `mod` modBase
                isCorrect = modExp leftNum exponent1 modBase == modExp rightNum exponent2 modBase

easyFractionPower :: Integer -> ChoiceTree [(Field, Bool)]
easyFractionPower modBase = Branch [ Branch [ g leftNum rightNum modBase | leftNum <- (filter (\x -> (x `mod` modBase == rightNum `mod` modBase) && x /=rightNum ) squares)] | rightNum <- squares]
    where
        g leftNum rightNum modBase = Node [(FMath ("\\sqrt{" ++ show leftNum ++ "}" ++  "\\  \\equiv_{" ++ show modBase ++ "} \\ \\ \\sqrt{" ++ show rightNum ++ "}"), isCorrect)]
            where
                isCorrect = (rootOfPerfectSquare leftNum) `mod` modBase == (rootOfPerfectSquare rightNum) `mod` modBase  

easyAddition :: Integer -> ChoiceTree [(Field, Bool)]
easyAddition modBase = Branch [ Branch [g leftNum summand modBase| summand <- [1..modBase*2] ] | leftNum <- getLeftNums modBase]
    where
        g leftNum summand modBase = Node [(FMath (show leftNum ++ " + " ++ show summand ++ "\\  \\equiv_{" ++ show modBase ++ "} \\ \\ " ++ show rightNum ++ " + " ++ show summand), True)]
            where
                rightNum = leftNum `mod` modBase

easyMultiplication :: Integer -> ChoiceTree [(Field, Bool)]
easyMultiplication modBase = Branch [ Branch [g leftNum factor modBase| factor <- [1..modBase*2] ] | leftNum <- getLeftNums modBase]
    where
        g leftNum factor modBase = Node [(FMath (show leftNum ++ " \\cdot " ++ show factor ++ "\\  \\equiv_{" ++ show modBase ++ "} \\ \\  " ++ show rightNum ++ " \\cdot " ++ show factor), True)]
            where
                rightNum = leftNum `mod` modBase

squares = [x*x | x <- [1..20]]
rootOfPerfectSquare :: Integer -> Integer
rootOfPerfectSquare i = 
    case index of
        Just index -> toInteger index
        Nothing -> -1
        where index = elemIndex i [x*x | x <- [1..20]]

-- taken from https://gist.github.com/trevordixon/6788535
modExp :: Integer -> Integer -> Integer -> Integer
modExp  x y n = mod (x^(mod y (n-1))) (n)

getLeftNums :: Integer -> [Integer]
getLeftNums modBase = (filter (\x -> x `mod` modBase /= 0)[modBase..modBase*5])

gen_multiplication :: String  -- always congruent
gen_multiplication = ""

gen_intPower :: String -- 
gen_intPower = ""

gen_fracPower :: String
gen_fracPower = ""

modNFeedback :: [(Field, Bool)] -> Map.Map String String -> ProblemResponse -> ProblemResponse
modNFeedback qa usr defaultRsp
    = reTime $ if (isCorrect) 
               then correct{prFeedback=[FText "correct, you beast"] }
               else wrong{prFeedback=[FText ("wrong"),table]}
                

    where
        table = FTable (
                        [[Header (FText "Equation"),Header (FText "Your Answer"), Header (FText "Solution") ]] ++ 
                        [[Cell question, Cell (FText (charToBoolStr sol)), Cell (FText $ show answer)]  | ((question, answer), sol) <- qAndA ]
                        )
        
        qAndA = zip qa sols

        answers = [show b | (a,b) <- qa]
        sols = [ solLookup i | i <- [1..2]]
        allAnsWithSol = zip answers sols
        isCorrect = all (==True) [ givenAns == (charToBoolStr solu) | (givenAns,solu) <- allAnsWithSol]

        charToBoolStr c = if(c == "F") then "False" else (if(c == "T") then "True" else "")

        solLookup i = case x of
                      Nothing -> ""
                      Just a -> a
                      where x = Map.lookup ("TF_"++ show i) usr 
    
        wrong = markWrong defaultRsp
        correct = markCorrect defaultRsp