module CS30.Exercises.ProbWord.Basics (basicprob) where
import           CS30.Data
import           CS30.Exercises.Data
import           GHC.Real -- for (%)


basicprob :: [ChoiceTree ([Field], Rational)]
basicprob = [ do blue <- nodes [1..8]
                 red <- nodes [1..8]
                 green <- nodes [1..8]
                 (colorNm,picked) <- nodes [ ("blue" ,blue )
                                           , ("red"  ,red  )
                                           , ("green",green)]
                 let tot = red+green+blue
                 let notpicked = tot - picked
                 nodes [ ( [FText $ "there are " ++ show blue ++ " blue marbles, " ++ show red ++ " red marbles, and " ++ show green ++ " green marbles, what is the probability of picking a "++colorNm++" marble?"]
                         , picked % tot
                         )
                       , ( [FText $ "there are " ++ show blue ++ " blue marbles, " ++ show red ++ " red marbles, and " ++ show green ++ " green marbles, what is the probability of picking a marble that isn't "++colorNm++"?"]
                         , notpicked % tot
                         )
                       ]
            , do blue <- nodes [2..8]
                 red <- nodes [2..8]
                 green <- nodes [2..8]
                 (colorNm,picked) <- nodes [ ("blue" ,blue )
                                           , ("red"  ,red  )
                                           , ("green",green)]
                 let tot = red+green+blue
                 let notpicked = tot - picked
                 let tot2 = (blue + red + green - 1)*(blue + red + green)
                 nodes [ ( [FText $ "there are " ++ show blue ++ " blue marbles, " ++ show red ++ " red marbles, and " ++ show green ++ " green marbles. Two are picked. What is the probability that both are "++colorNm++" marbles?"]
                         , (picked * (picked - 1)) % tot2
                         )
                       , ( [FText $ "there are " ++ show blue ++ " blue marbles, " ++ show red ++ " red marbles, and " ++ show green ++ " green marbles. Two are picked. What is the probability that neither is a "++colorNm++" marble?"]
                         , ((notpicked - 1)*notpicked) % tot2
                         )
                       ]
            ]
