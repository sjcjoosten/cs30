module CS30.Exercises (pages) where
import CS30.Exercises.Cardinality (cardEx)
import CS30.Exercises.CombinatoricsIntegers (combinEx)
import CS30.Exercises.ComputeX (modsEx)
import CS30.Exercises.Data (ExerciseType(etTotal))
import CS30.Exercises.Graphs (graphStub, giveSet)
import CS30.Exercises.IncExcCardinalities (incExcCardinalitiesEx)
import CS30.Exercises.LogicExpr.CreativeDisplay (logicWrongStepEx)
import CS30.Exercises.LogicExpr.Display (logicProofOrderEx)
import CS30.Exercises.LogicInequalities (logicInequalitiesEx)
import CS30.Exercises.LogicRewriting.Exercise (logicRewritingEx)
import CS30.Exercises.ModN (modN)
import CS30.Exercises.ModuloGenerateEx (modProofEx)
import CS30.Exercises.ProbExProof (probExProof)
import CS30.Exercises.ProbWord ( probBasicEx, probExpectEx )
import CS30.Exercises.Probability (probaEx)
import CS30.Exercises.SetBasics (rosterEx, powsetEx, setOpsEx)
import CS30.Exercises.SetCardinalitiesProofs.ExerciseGeneration (cardinalityProofExer)
import CS30.Exercises.SetConversionProofs.SetConversion (setConversionEx)
import CS30.Exercises.Table (tableStub)
import CS30.Exercises.TruthTable (truthEx)

-- a note on MathQuill (what LaTeX is valid and what LaTeX is not):
-- http://math.chapman.edu/~jipsen/mathquill/test/MathQuillsymbolsMathJax.html
-- http://math.chapman.edu/~jipsen/mathquill/test/test.html

pages :: [ExerciseType]
pages = [ rosterEx
        , powsetEx{etTotal = 6}
        , setOpsEx -- from SetBasics

        , setConversionEx -- set-builder notation rewrite proofs
        , incExcCardinalitiesEx -- from IncExcCardinalities (Rachael and Tyler)
        , cardEx -- Bennett and Donia, from set cardinalities

        , logicRewritingEx -- Chibuzo and Bennett: select the name of the law that is named
        , logicProofOrderEx{etTotal = 6} -- Tyler and Fei: put proof in right order
        , logicWrongStepEx{etTotal = 4} -- Tyler and Fei: which steps are correct and which are not?
        -- , logicInequalitiesEx -- Kyle and Lucas: Logic; inequality problems. Wrong method to generate proofs

        , probBasicEx -- Fei and Kyle wk 5/6 probability: basic probability (solvable through counting)
        , probaEx -- Probability, compute expression. Contains conditional probabilities and independent stuff
        , probExpectEx  -- Fei and Kyle wk 5/6 probability: Expected value exercises
        -- , giveSet -- graph basics (needs to be disentangled and edges need to be shown/typed properly)
        -- , tableStub -- does not pass tests, since it's not a valid exercise, but uncomment to see how tables are displayed.
        -- , truthEx -- Anmol and Sanket compute a truth table, sometimes generates impossible questions
        -- , modsEx -- Modular Arithmetic exercises wk 5/6. Maha and Roberto? Needs to be disentangled
        -- , modN -- modulo N, true or false. Needs fixing
        -- , cardinalityProofExer -- had negative exponents errors, needs fixing
        -- , modProofEx -- confusing step-names
        -- , combinEx -- doesn't work
        -- , probExProof -- works, but questions look ugly
        ]

-- the definition below is to prevent warnings about unused imports
_ignorable :: [ExerciseType]
_ignorable = [graphStub, tableStub, combinEx, probExProof, truthEx, giveSet, modsEx, modN, cardinalityProofExer, modProofEx, logicInequalitiesEx]
