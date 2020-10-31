module CS30.Exercises.ProofStub (proofStub) where
import CS30.Data
import CS30.Exercises.Data
import qualified Data.Map as Map

permutations :: Int -> ChoiceTree [Int]
permutations 0 = Node []
permutations n -- ChoiceTree is a Monad now! I've also derived "Show", so you can more easily check this out in GHCI.
 = do i <- nodes [0..n-1]
      rm <- permutations (n-1)
      return (i : map (\v -> if v >= i then v+1 else v) rm)

breakUnderscore :: String -> [String]
breakUnderscore s
  =  case dropWhile (=='_') s of
       "" -> []
       s' -> w : breakUnderscore s''
             where (w, s'') = break (=='_') s'

proofStub :: ExerciseType
proofStub = exerciseType "proofStub" "(testing)" "Displaying sortable proofs" 
              [permutations 5] genProof simpleFeedback
        where simpleFeedback sol rsp pr
               = case Map.lookup "proof" rsp of
                   Just str
                     -> if map ((sol !!) . read) (breakUnderscore str) == [0..4]
                        then markCorrect pr
                        else markWrong pr{prFeedback=[FText "You answered: ",FText str]}
                   Nothing -> error "Client response is missing 'proof' field"
genProof :: [Int] -> Exercise -> Exercise
genProof order def 
 = def{ eQuestion = [ FText $"Here is an example proof, can you put it in the right order?"
                    , FIndented 1 [FMath "(x + y)^2"]
                    , FReorder "proof"
                        (map ([step1,step2,step3,step4,step5] !!) order)
                    ]
      , eBroughtBy = ["Sebastiaan Joosten"] }
 where 
   step1 = [ FMath "=", FText "{ definition of taking a square }"
           , FIndented 1 [FMath "(x + y) \\cdot (x + y)"] ]
   step2 = [ FMath "=", FText "{ distributivity of multiplication over addition }"
           , FIndented 1 [FMath "x \\cdot (x + y) + y \\cdot (x + y)"] ]
   step3 = [ FMath "=", FText "{ distributivity of multiplication over addition (applied twice) }"
           , FIndented 1 [FMath "x \\cdot x + x \\cdot y + y \\cdot x + y \\cdot y"] ]
   step4 = [ FMath "=", FText "{ definition of taking a square }"
           , FIndented 1 [FMath "x^2 + x \\cdot y + y \\cdot x + y^2"] ]
   step5 = [ FMath "=", FText "{ combining equal values }"
           , FIndented 1 [FMath "x^2 + 2xy + y^2"] ]