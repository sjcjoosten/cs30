import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void

type Parser   = Parsec Void String
data Law      = Law LawName Equation
type LawName  = String
type Equation = (Expr,Expr)
data Expr     = Var String | Const Integer | Op Op [Expr] deriving (Eq, Show)
data Op       = Mul | Div | Add | Sub | Exp | Fac deriving (Eq, Show)
data Proof    = Proof Expr [(String, Expr)]

-- law :: Parser Law
-- law = do {name <- upto ':';
--           eqn <- equation; return (Law name eqn)}

-- upto :: Char -> Parser String upto c
-- = Parser (\s ->
-- let (xs,ys) = break (==c) s in
--                         if null ys then []
--                         else [(xs,tail ys)])

instance Show Law where 
  showsPrec _ (Law name (e1,e2))
    = showString name . 
      showString ": " . 
      shows e1 . 
      showString " = " . 
      shows e2


getProofLengthN :: Int -> [Law] -> (Expr -> Law -> [Expr] ) -> Expr -> [Proof]
getProofLengthN 0 _ _ e = [Proof e []]
getProofLengthN n laws fnc e
  = [ Proof e ((lawName,e'):steps)
    | (Law lawName eqn) <- laws
    , e' <- fnc e (Law lawName eqn)
    , (Proof _ steps) <- getProofLengthN (n-1) laws fnc e']
  -- where go exp = case [(lawName,res) | (Law lawName eqn) <- laws, res <- fnc exp (Law lawName eqn)] of 
  --                 [] -> []
  --                 (step@(lname,res):_) -> step:go res


parseLaw :: Parser Expr -> Parser Law
parseLaw parseExpr = do lawName <- parseUntil ':'
                        _ <- string ":"
                        lhs <- parseExpr
                        _ <- string "="
                        rhs <- parseExpr
                        return (Law lawName (lhs,rhs))

parseUntil :: Char -> Parser String
parseUntil c = (do _ <- satisfy (== c)
                   return  []) <|>
               (do c <- satisfy (const True)
                   rmd <- parseUntil c
                   return (c:rmd)
               )
