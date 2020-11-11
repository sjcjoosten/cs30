module CS30.Exercises.LogicExpr.Test where
-- import CS30.Exercises.LogicExpr.Parser
-- import CS30.Exercises.LogicExpr.Proof

-- simplify input_laws "¬(p ∧ q) ∧ (¬p ∨ q) ∧ (¬q ∨ q)"
-- Result:
-- Proof ((¬('p' ∧ 'q') ∧ (¬'p' ∨ 'q')) ∧ (¬'q' ∨ 'q')) [("Law10",((¬'p' ∨ ¬'q') ∧ (¬'p' ∨ 'q')) ∧ (¬'q' ∨ 'q')),("Law12",((¬'p' ∨ ¬'q') ∧ (¬'p' ∨ 'q')) ∧ True),("Law3",(¬'p' ∨ ¬'q') ∧ (¬'p' ∨ 'q')),("Redundancy law",¬'p')]

-- Too long proof, many Assoc, but otherwise can't reduce to simpliest 
-- simplify input_laws "¬(p ⇒ q) ⇒ (p ∧ ¬q)"
-- Result:
-- Proof (¬('p' ⇒ 'q') ⇒ ('p' ∧ ¬'q')) [("Law6",¬(¬('p' ⇒ 'q')) ∨ ('p' ∧ ¬'q')),("Law1",('p' ⇒ 'q') ∨ ('p' ∧ ¬'q')),("Law6",(¬'p' ∨ 'q') ∨ ('p' ∧ ¬'q')),("Assoc",(('p' ∧ ¬'q') ∨ ¬'p') ∨ 'q'),("Assoc",('q' ∨ ('p' ∧ ¬'q')) ∨ ¬'p'),("Redundancy law",('q' ∨ 'p') ∨ ¬'p'),("Assoc",(¬'p' ∨ 'q') ∨ 'p'),("Assoc",('p' ∨ ¬'p') ∨ 'q'),("Law12",True ∨ 'q'),("Law4",True)]

-- ???
-- We need Associative and Commutative laws to deal with this 
-- simplify input_laws "q∧p∧q"
-- With commutativity built-in
-- Step 2 is generated because of implicit commutativity combined with associativity
-- Proof ((q ∧ p) ∧ q) [("Associativity3",q ∧ (p ∧ q)),("Associativity3",p ∧ (q ∧ q)),("Idempotence",p ∧ q)]
-- With explicit commutativity laws,
-- ghci>simplify input_laws "q∧p∧q"
-- Proof ((q ∧ p) ∧ q) [("Associativity3",q ∧ (p ∧ q)),("Associativity4",(q ∧ p) ∧ q),("Commutativity",q ∧ (q ∧ p)),("Associativity4",(q ∧ q) ∧ p),("Idempotence",q ∧ p),("Commutativity",p ∧ q)]
-- Previous result with commutativity laws before associative laws
-- Result:
-- Proof (('q' ∧ 'p') ∧ 'q') [("Commute",'q' ∧ ('q' ∧ 'p')),("Commute",('q' ∧ 'p') ∧ 'q'),("Assoc",('q' ∧ 'q') ∧ 'p'),("Law2",'q' ∧ 'p'),("Commute",'p' ∧ 'q')]

-- simplify input_laws "(¬p ∨ ¬q) ∧ (¬p ∨ q)""
-- simplify input_laws "¬p∨¬q∧(¬p∨q)"
-- = ¬p

-- Commutative Test
-- simplify input_laws "¬p∨p"
-- True

-- exp2 = parseExpr "(p∨p)"
-- exp3 = parseExpr "p⇒r"
-- exp4 = parseExpr "¬(p ⇒ q) ⇒ (p ∧ ¬q)"
-- exp4 = parseExpr "false∧q"

