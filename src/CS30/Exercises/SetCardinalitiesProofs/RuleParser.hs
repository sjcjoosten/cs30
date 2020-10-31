
data Symb = Add | Minus | Intersection | Union | Powerset | Card | Const Char
data Expr = Var Symb | Op Symb [Expr]

data Law = Law String Equation
type Equation = (Expr, Expr)  -- (left,right)

-- Given Rules:
-- |A \cup B| = |A| + |B| - |A \cap B|
-- |A \times B| = |A| \cdot |B|
-- |\P(A)| = 2^{|A|}
-- |A \setminus B| = |A| - |A \cap B|
