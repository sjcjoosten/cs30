module CS30.Exercises (pages) where
import CS30.Exercises.Data (ExerciseType)
import CS30.Exercises.SetBasics (rosterEx, powsetEx, setOpsEx)


-- a note on MathQuill (what LaTeX is valid and what LaTeX is not):
-- http://math.chapman.edu/~jipsen/mathquill/test/MathQuillsymbolsMathJax.html
-- http://math.chapman.edu/~jipsen/mathquill/test/test.html

pages :: [ExerciseType]
pages = [ rosterEx, powsetEx, setOpsEx -- from SetBasics
        ]






