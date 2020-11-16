module CS30.Exercises (pages) where
import CS30.Exercises.Cardinality (cardEx)
import CS30.Exercises.ComputeX (modsEx)
import CS30.Exercises.Data (ExerciseType)
import CS30.Exercises.Graphs (graphStub)
import CS30.Exercises.CombinatoricsIntegers (combinEx)
import CS30.Exercises.IncExcCardinalities (incExcCards)
import CS30.Exercises.ModN (modN)
import CS30.Exercises.ProbWord ( probBasicEx, probExpectEx )
import CS30.Exercises.Probability (probaEx)
import CS30.Exercises.SetBasics (rosterEx, powsetEx, setOpsEx)
import CS30.Exercises.Table (tableStub)
import CS30.Exercises.TruthTable (truthEx)
import CS30.Exercises.ProofStub (proofStub)
import CS30.Exercises.GenerateExerViaProofs.ExpreParser

-- a note on MathQuill (what LaTeX is valid and what LaTeX is not):
-- http://math.chapman.edu/~jipsen/mathquill/test/MathQuillsymbolsMathJax.html
-- http://math.chapman.edu/~jipsen/mathquill/test/test.html

pages :: [ExerciseType]
pages = [ rosterEx, powsetEx, setOpsEx -- from SetBasics
        , probBasicEx, probExpectEx  -- Probability: Word Problems, basic probability and expected value exercises
        -- , graphStub -- does not pass tests, since it's not a valid exercise. It's also not yet implemented on the frontend.
        -- , tableStub -- does not pass tests, since it's not a valid exercise, but uncomment to see how tables are displayed.
        , proofStub -- does not pass tests, since it's not a valid exercise, but uncomment to see how re-sortable proofs can be displayed.
        , truthEx
        , modsEx -- Modular Arithmetic exercises
        , cardEx -- from Cardinality
        , incExcCards -- from IncExcCardinalities
        , probaEx
        , modN
        , probabilityProof 
        -- , combinEx -- too slow, see TODO
        ]

-- the definition below is to prevent warnings about unused imports
_ignorable :: [ExerciseType]
_ignorable = [graphStub, tableStub, combinEx]
