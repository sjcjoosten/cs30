module CS30.Exercises.ProbWord.Basics (basicprob) where
import           CS30.Data
import           CS30.Exercises.Data


basicprob :: [ChoiceTree ([Field], String)]
basicprob = [ node [ ( [show numDice ++ " fair dice are rolled, find the probability that the sum of the results is less than " ++ show sum
                        | numDice <- [(2::Int)..4], sum <- [numDice + 1..numDice*6]]
                     -- , FFieldMath "prob" 
                     , "1/54"
                      )
                    ]          
           ]