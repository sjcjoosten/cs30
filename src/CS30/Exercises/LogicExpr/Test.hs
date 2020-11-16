module CS30.Exercises.LogicExpr.Test where
import CS30.Exercises.LogicExpr.Parser
import CS30.Exercises.LogicExpr.Proof

sampleSimplify :: IO ()
sampleSimplify = do
    print $ simplify input_laws "¬(p ∧ q) ∧ (¬p ∨ q) ∧ (¬q ∨ q)"    
    print $ simplify input_laws "¬(p ⇒ q) ⇒ (p ∧ ¬q)"    
    print $ simplify input_laws "q∧p∧q"    
    print $ simplify input_laws "¬p∨p"


showExprTests :: IO ()
showExprTests = do 
    print $ (show (Bin And (Bin And (Bin And (Con True) (Var 'r')) (Var 'q')) (Var 'p'))) == "true ∧ r ∧ q ∧ p"    
    print $ (show (Neg (Neg (Bin And (Var 'q') (Var 'p'))))) == "¬¬(q ∧ p)"    
    print $ (show (Neg (Neg (Var 'p')))) == "¬¬p"    
    print $ (show (Bin Imply (Bin Imply (Var 'p') (Var 'q')) (Bin Imply (Var 'q') (Var 'r')))) == "(p ⇒ q) ⇒ (q ⇒ r)"
