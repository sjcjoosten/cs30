module CS30.Exercises.GenerateExerViaProofs.ExpreParser where
import CS30.Exercises.GenerateExerViaProofs.ProofProbability
import CS30.Exercises.Data
import CS30.Data
import CS30.Exercises.GenerateExerViaProofs.GenerateProof
import Text.Megaparsec ((<|>))

probabilityProof :: ExerciseType
probabilityProof 
    = exerciseType  "probabilityProof"
                    "L?.?"
                    "Probability: generating exercises via proofs" 
                    [(generateRandEx 3),(generateRandEx 5)]
                    genEveExper 
                    generateFeedback

generateRandEx :: Int -> ChoiceTree (FracExpr, [[Field]], [Rational])
generateRandEx n = undefined

-- generate problems for tests
generateRandExH :: Int -> ChoiceTree FracExpr
generateRandExH i 
    | i < 1 = Branch [Branch [Node (FVar varName) | varName <-['A','B','C']]         
        , Branch [Node (FConst 0) , Node (FConst 1) ]]
generateRandExH i 
    = Branch  [do {e1 <- generateRandExH i';           
                   e2 <- generateRandExH (i - i' - 1);
                   opr <- nodes [Plus,Minus,Divide,Mult, ];
                   return (FBin opr e1 e2)}    
                | i' <- [0..i-1]         
                ]


example1, example2, example3, example4, example5, example6 :: FracExpr
example1 = FConst 0
example2 = AndEvent (FVar 'A') (FVar 'B')
example3 = OrEvent (FVar 'A') (FVar 'B')
example4 = NegaEvent (FVar 'A')
example5 = FVar 'C'
example6 = NegaEvent (AndEvent (FVar 'A') (FVar 'B'))

-- generate two event expression from a set of base events

genEveExper = undefined


generateFeedback = undefined

