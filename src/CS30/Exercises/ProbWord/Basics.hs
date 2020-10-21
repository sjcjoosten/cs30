module CS30.Exercises.ProbWord.Basics (basicprob) where
import           CS30.Data
import           CS30.Exercises.Data
import GHC.Real -- for (%)

basicprob :: [ChoiceTree ([Field], Rational)]
basicprob = [ Branch [ nodes [ ( [FText $ show numDice ++ " fair dice are rolled, find the probability that the sum of the results is less than " ++ show sum, FFieldMath "prob"]
                              , 1 % 54
                                )
                              , ( [FText $ show numDice ++ " fair dice are rolled, find the probability that the sum of the results is equal to " ++ show sum, FFieldMath "prob"]
                                , 1 % 54
                                )
                              ]
                              | numDice <- [(2::Integer)..4], sum <- [numDice + 1..numDice*6 - 1]
                              ]
            , Branch [ nodes [ ( [FText $ "there are " ++ show blue ++ " blue marbles, " ++ show red ++ " red marbles, and " ++ show green ++ " green marbles, what is the probability of picking a blue marble?", FFieldMath "prob"]
                                , blue % (blue + red + green)
                                )
                              , ( [FText $ "there are " ++ show blue ++ " blue marbles, " ++ show red ++ " red marbles, and " ++ show green ++ " green marbles, what is the probability of picking a marble that isn't blue?", FFieldMath "prob"]
                                , (red + green) % (blue + red + green)
                                )
                              ]
                              | blue <- [(1::Integer)..8], red <- [(1::Integer)..8], green <- [(1::Integer)..8] ]
            , Branch [ nodes [ ( [FText $ "you invest $100 in a stock that will return $" ++ show winVal ++ " with probability " ++ show winProb ++ " or $" ++ show loseVal ++ " with probability " ++ show (1 - winProb) ++ ", what is your expected profit?", FFieldMath "prob"]
                                , winVal * winProb + loseVal * (1 - winProb) - 100
                                )
                              | winVal <- [120,130..180], winProb <- [0.2,0.3..0.5], loseVal <- [50,55..90] ]
                              ]
           ]