module CS30.Exercises (pages) where
import CS30.Exercises.Cardinality (cardEx)
import CS30.Exercises.ComputeX (modsEx)
import CS30.Exercises.Data (ExerciseType)
import CS30.Exercises.Graphs (graphStub, giveSet)
import CS30.Exercises.CombinatoricsIntegers (combinEx)
import CS30.Exercises.IncExcCardinalities (incExcCards)
import CS30.Exercises.ModN (modN)
import CS30.Exercises.ProbWord ( probBasicEx, probExpectEx )
import CS30.Exercises.Probability (probaEx)
import CS30.Exercises.SetBasics (rosterEx, powsetEx, setOpsEx)
import CS30.Exercises.Table (tableStub)
import CS30.Exercises.TruthTable (truthEx)
import CS30.Exercises.SetCardinalitiesProofs.ExerciseGeneration (cardinalityProofExer)
import CS30.Exercises.ProbExProof (probExProof)
import CS30.Exercises.ModuloGenerateEx (modProofEx)
import CS30.Exercises.LogicRewriting.Exercise (logicRewritingEx)
import CS30.Exercises.LogicExpr.Display (logicProof)
import CS30.Exercises.SetConversionProofs.SetConversion (setConv)
import CS30.Exercises.LogicInequalities (logicInequalitiesEx)
import CS30.Exercises.LogicExpr.CreativeDisplay (logicWrongStepEx)
import CS30.Exercises.GenerateExerViaProofs.ExpreParser ( probabilityProof )

-- a note on MathQuill (what LaTeX is valid and what LaTeX is not):
-- http://math.chapman.edu/~jipsen/mathquill/test/MathQuillsymbolsMathJax.html
-- http://math.chapman.edu/~jipsen/mathquill/test/test.html

pages :: [ExerciseType]
pages = [ rosterEx, powsetEx, setOpsEx -- from SetBasics
        , probBasicEx, probExpectEx  -- Probability: Word Problems, basic probability and expected value exercises
        , giveSet
        -- , tableStub -- does not pass tests, since it's not a valid exercise, but uncomment to see how tables are displayed.
        , truthEx
        , modsEx -- Modular Arithmetic exercises
        , cardEx -- from Cardinality
        , incExcCards -- from IncExcCardinalities
        , probaEx
        , modN
        , cardinalityProofExer -- has negative exponents errors
        , modProofEx
        , logicRewritingEx
        , logicProof
        , logicInequalitiesEx -- Logic; inequality problems.
        , logicWrongStepEx
        , probabilityProof 
        -- , combinEx -- too slow, see TODO
        , probExProof
        , setConv
        ]

-- the definition below is to prevent warnings about unused imports
_ignorable :: [ExerciseType]
_ignorable = [graphStub, tableStub, combinEx]
