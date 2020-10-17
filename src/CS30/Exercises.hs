module CS30.Exercises (pages) where
import CS30.Exercises.Data (ExerciseType)
import CS30.Exercises.SetBasics (rosterEx, powsetEx, setOpsEx)
import CS30.Exercises.Graphs (graphStub)
import CS30.Exercises.Table (tableStub)
import CS30.Exercises.ComputeX (modsEx)

-- a note on MathQuill (what LaTeX is valid and what LaTeX is not):
-- http://math.chapman.edu/~jipsen/mathquill/test/MathQuillsymbolsMathJax.html
-- http://math.chapman.edu/~jipsen/mathquill/test/test.html

pages :: [ExerciseType]
pages = [ rosterEx, powsetEx, setOpsEx -- from SetBasics
        -- , graphStub -- does not pass tests, since it's not a valid exercise. It's also not yet implemented on the frontend.
        -- , tableStub -- does not pass tests, since it's not a valid exercise, but uncomment to see how tables are displayed.
        , modsEx
        ]

-- the definition below is to prevent warnings about unused imports
_ignorable :: [ExerciseType]
_ignorable = [graphStub, tableStub]





