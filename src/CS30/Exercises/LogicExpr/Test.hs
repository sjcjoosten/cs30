module Test where
import CS30.Exercises.LogicExpr.Parser
import CS30.Exercises.LogicExpr.Logic

-- simplify input_laws "¬(p ∧ q) ∧ (~p ∨ q) ∧ (¬q ∨ q)"
-- Result:
-- Proof ((¬('p' ∧ 'q') ∧ (¬'p' ∨ 'q')) ∧ (¬'q' ∨ 'q')) [("Law10",((¬'p' ∨ ¬'q') ∧ (¬'p' ∨ 'q')) ∧ (¬'q' ∨ 'q')),("Law12",((¬'p' ∨ ¬'q') ∧ (¬'p' ∨ 'q')) ∧ True),("Law3",(¬'p' ∨ ¬'q') ∧ (¬'p' ∨ 'q')),("Redundancy law",¬'p')]

-- simplify input_laws "¬(p ⇒ q) ⇒ (p ∧ ¬q)"
-- Result:
-- Proof (¬('p' ⇒ 'q') ⇒ ('p' ∧ ¬'q')) [("Law6",¬(¬('p' ⇒ 'q')) ∨ ('p' ∧ ¬'q')),("Law1",('p' ⇒ 'q') ∨ ('p' ∧ ¬'q')),("Law6",(¬'p' ∨ 'q') ∨ ('p' ∧ ¬'q')),("Assoc",(('p' ∧ ¬'q') ∨ ¬'p') ∨ 'q'),("Assoc",('q' ∨ ('p' ∧ ¬'q')) ∨ ¬'p'),("Redundancy law",('q' ∨ 'p') ∨ ¬'p'),("Assoc",(¬'p' ∨ 'q') ∨ 'p'),("Assoc",('p' ∨ ¬'p') ∨ 'q'),("Law12",True ∨ 'q'),("Law4",True)]

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

