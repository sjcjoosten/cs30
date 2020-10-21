module CS30.Exercises.ProbWord.Basics (basicprob) where
import           CS30.Data
import           CS30.Exercises.Data
import GHC.Real -- for (%)


basicprob :: [ChoiceTree ([Field], Rational)]
basicprob = [ nodes [([FText $ show numDice ++ " fair dice are rolled, find the probability that the sum of the results is less than " ++ show sum, FFieldMath "prob"], 1 % 54)
                        | numDice <- [(2::Int)..4], sum <- [numDice + 1..numDice*6]]
                     
           ]