module CS30.Exercises.ProbWord.Basics (basicprob) where
import           CS30.Data
import           CS30.Exercises.Data


basicprob :: [ChoiceTree ([Field], String)]
basicprob = [ nodes [ ( [FText "3 fair dice are rolled, find the probability that the sum of the results is less than 5", FFieldMath "prob"]
                     , "1/54"
                      )
                    ]          
           ]