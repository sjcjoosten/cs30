module CS30.Exercises.ProbWord.ExpectedValue (expectprob) where
import           CS30.Data
import           CS30.Exercises.Data
import           Numeric

expectprob :: [ChoiceTree ([Field], Double)]
expectprob = [ Branch [ Branch [nodes [ ( [FText $ "you invest $100 in a stock that will return $" ++ show winVal ++ " with probability " ++ (showFFloat (Just 1) winProb "") ++ " or $" ++ show loseVal ++ " with probability " ++ (showFFloat (Just 1) (1 - winProb) "") ++ ", what is your expected return?"]
                                        , fromInteger winVal * winProb + fromInteger loseVal * (1 - winProb)
                                        )
                                      | winVal <- [120,130..180]]
                               | winProb <- [0.2,0.3..0.5]]
                      | loseVal <- [50,55..90]]
             , Branch [ Branch [nodes [ ( [FText $ "a raffle ticket pays out $" ++ show firstPlaceVal ++ " with probability " ++ (showFFloat (Just 3) firstPlaceProb "") ++ " or $" ++ show secondPlaceVal ++ " with probability " ++ (showFFloat (Just 3) secondPlaceProb "") ++ ", what is a fair price for this ticket? (One where there is no net win/loss)"]
                                        , (fromInteger firstPlaceVal * firstPlaceProb + fromInteger secondPlaceVal * secondPlaceProb) :: Double
                                        )
                                      | secondPlaceVal <- [300,600..(firstPlaceVal*2) `div` 3]
                                      ]
                               | firstPlaceVal <- [1000,1500..5000]
                               ] 
                       | firstPlaceProb <- [0.001,0.002..0.005]
                       , secondPlaceProb <- [0.006,0.007..0.010]
                       ]
              , Branch [ Branch [nodes [ ( [FText $ show numBets ++ " people place $100 bets that will pay out $" ++ show winVal ++ " with probability " ++ (showFFloat (Just 1) winProb "") ++ ", how much should the casino expect to make?"]
                                         , fromInteger numBets * (100 - fromInteger winVal * winProb)
                                         )
                                       | winProb <- filter (\x -> fromInteger winVal * x < 100) [0.1,0.2,0.3,0.4]
                                       ]
                                | numBets <- [40,50..90]
                                ]
                       | winVal <- [200,300..900]
                       ]
            ]
