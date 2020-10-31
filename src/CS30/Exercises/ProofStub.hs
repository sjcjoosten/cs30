module CS30.Exercises.ProofStub (proofStub) where
import CS30.Data
import CS30.Exercises.Data
import qualified Data.Map as Map

proofStub :: ExerciseType
proofStub = exerciseType "proofStub" "(testing)" "Displaying sortable proofs" 
              [boolTree] genProof simpleFeedback
        where simpleFeedback _ rsp pr
               = case Map.lookup "proof" rsp of
                   Just "1_2_0_4_3" -> markCorrect pr
                   Just v -> markWrong pr{prFeedback=[FText "You answered: ",FText v]}
                   Nothing -> error "Client response is missing 'proof' field"
genProof :: a -> Exercise -> Exercise
genProof _ def 
 = def{ eQuestion = [ FText $"Here is an example proof, can you put it in the right order?"
                    , FIndented 1 [FMath "(x + y)^2"]
                    , FReorder "proof" [step3,step1,step2,step5,step4]
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