module CS30.Exercises.ProbWord.ExpectedValue (expectprob) where
import           CS30.Data
import           CS30.Exercises.Data
import           Numeric


expectprob :: [ChoiceTree ([Field], Double)]
expectprob = [ Branch [ nodes [ ( [FText $ "you invest $100 in a stock that will return $" ++ show (floor winVal) ++ " with probability " ++ (showFFloat (Just 1) winProb "") ++ " or $" ++ show (floor loseVal) ++ " with probability " ++ (showFFloat (Just 1) (1 - winProb) "") ++ ", what is your expected return?"]
                                , winVal * winProb + loseVal * (1 - winProb)
                                )
                              | winVal <- [120::Double,130..180]
                              , winProb <- [0.2,0.3..0.5]
                              , loseVal <- [50,55..90] 
                             ]
                     ]
              , Branch [ nodes [ ( [FText $ "a raffle ticket pays out $" ++ show (floor firstPlaceVal) ++ " with probability " ++ (showFFloat (Just 3) firstPlaceProb "") ++ " or $" ++ show (floor secondPlaceVal) ++ " with probability " ++ (showFFloat (Just 3) secondPlaceProb "") ++ ", what is a fair price for this ticket?"]
                                , firstPlaceVal * firstPlaceProb + secondPlaceVal * secondPlaceProb
                                )
                              | firstPlaceVal <- [1000::Double,1500..5000]
                              , secondPlaceVal <- [(firstPlaceVal*0.3),(firstPlaceVal*0.3 + 300)..firstPlaceVal*0.6]
                              , firstPlaceProb <- [0.001,0.002..0.005]
                              , secondPlaceProb <- [0.006,0.007..0.010]
                             ]
                        ]
              , Branch [ nodes [ ( [FText $ show numBets ++ " people place $100 bets that will pay out $" ++ show (floor winVal) ++ " with probability " ++ (showFFloat (Just 1) winProb "") ++ ", how much should the casino expect to make?"]
                                , numBets * (100 - winVal * winProb)
                                )
                              | numBets <- [40,50..90]
                              , winVal <- [200::Double,300..900]
                              , winProb <- filter (\x -> winVal * x < 100) [0.1,0.2,0.3,0.4]
                             ]
                        ]
            ]
