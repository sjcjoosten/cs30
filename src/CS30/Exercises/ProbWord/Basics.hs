module CS30.Exercises.ProbWord.Basics (basicprob) where
import           CS30.Data
import           CS30.Exercises.Data
import GHC.Real -- for (%)
import CS30.Exercises.Util (dispRat)

-- sumList :: [Integer] -> Integer


basicprob :: [ChoiceTree ([Field], (Rational,String))]
basicprob = [ Branch [ nodes [ ( [FText $ show numDice ++ " fair dice are rolled, find the probability that the sum of the results is less than " ++ show sumDice, FFieldMath "prob"]
                              , ((foldr (+) 0 $ if numDice == 2 then take (fromIntegral sumDice-2)[1,2,3,4,5,6,5,4,3,2,1] else take (fromIntegral sumDice-3) [1,3,6,10,15,21,25,27,27,25,21,15,10,6,3,1]) % (6 ^ numDice), "frac")
                                )
                              , ( [FText $ show numDice ++ " fair dice are rolled, find the probability that the sum of the results is equal to " ++ show sumDice, FFieldMath "prob"]
                                , ((if numDice == 2 then [1,2,3,4,5,6,5,4,3,2,1] !! (fromIntegral sumDice-2) else [1,3,6,10,15,21,25,27,27,25,21,15,10,6,3,1] !! (fromIntegral sumDice-3)) % (6 ^ numDice), "frac")
                                )
                              ]
                              | numDice <- [(2::Integer)..3], sumDice <- [numDice + 1..numDice*6 - 1]
                              ]
            , Branch [ nodes [ ( [FText $ "there are " ++ show blue ++ " blue marbles, " ++ show red ++ " red marbles, and " ++ show green ++ " green marbles, what is the probability of picking a blue marble?", FFieldMath "prob"]
                                , (blue % (blue + red + green), "frac")
                                )
                              , ( [FText $ "there are " ++ show blue ++ " blue marbles, " ++ show red ++ " red marbles, and " ++ show green ++ " green marbles, what is the probability of picking a marble that isn't blue?", FFieldMath "prob"]
                                , ((red + green) % (blue + red + green), "frac")
                                )
                              ]
                              | blue <- [(1::Integer)..8], red <- [(1::Integer)..8], green <- [(1::Integer)..8] 
                              ]
            , Branch [ nodes [ ( [FText $ "you invest $100 in a stock that will return " ++ (dispRat winVal "money") ++ " with probability " ++ (dispRat winProb "dec") ++ " or " ++ (dispRat loseVal "money") ++ " with probability " ++ (dispRat (1 - winProb) "dec") ++ ", what is your expected profit?", FFieldMath "prob"]
                                , (winVal * winProb + loseVal * (1 - winProb) - 100, "money")
                                )
                              | winVal <- [120,130..180], winProb <- [0.2,0.3..0.5], loseVal <- [50,55..90] ]
                              ]
           ]