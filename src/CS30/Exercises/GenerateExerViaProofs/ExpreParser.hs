module CS30.Exercises.GenerateExerViaProofs.ExpreParser where
import CS30.Exercises.GenerateExerViaProofs.ProofProbability
import CS30.Exercises.Data (ChoiceTree)

-- generate two event expression from a set of base events
genEveExper :: FracExpr -> (FracExpr, FracExpr)
genEveExper = undefined

-- generate problems for tests
generateRandEx :: Int -> ChoiceTree FracExpr
generateRandEx = undefined

example1, example2, example3, example4, example5, example6 :: FracExpr
example1 = FConst 0
example2 = AndEvent (FVar 'A') (FVar 'B')
example3 = OrEvent (FVar 'A') (FVar 'B')
example4 = NegaEvent (FVar 'A')
example5 = FVar 'C'
example6 = NegaEvent (AndEvent (FVar 'A') (FVar 'B'))