module CS30.Exercises.ProbWord.ExpectedValue (expectprob) where
import           CS30.Data
import           CS30.Exercises.Data
import           Numeric


expectprob :: [ChoiceTree ([Field], Double)]
expectprob = [ Branch [ nodes [ ( [FText $ "you invest $100 in a stock that will return $" ++ show winVal ++ " with probability " ++ (showFFloat (Just 1) winProb "") ++ " or $" ++ show loseVal ++ " with probability " ++ (showFFloat (Just 1) (1 - winProb) "") ++ ", what is your expected profit?", FFieldMath "prob"]
                                , winVal * winProb + loseVal * (1 - winProb) - 100
                                )
                              | winVal <- [120::Double,130..180]
                              , winProb <- [0.2,0.3..0.5]
                              , loseVal <- [50,55..90] 
                             ]
                     ]
            ]
