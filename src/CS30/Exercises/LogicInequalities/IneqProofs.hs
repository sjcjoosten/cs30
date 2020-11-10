-- module CS30.Exercises.IneqProofs(ineqProofExercise) where
-- import CS30.Data
-- import CS30.Exercises.Data
-- import           CS30.Data
-- import           CS30.Exercises.Data
import           Control.Monad.Combinators.Expr
import qualified Data.Map as Map
import           Data.Void
import           Data.Either
import           Data.Maybe
import           Text.Megaparsec
import           Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L

-- defining datas and types
type Parser     = Parsec Void String

-- MathExpr for parsing, converted to Expr for everything else
data MathExpr  = MathConst Integer 
               | MathVar String
               | Fact MathExpr             -- factorial
               | Divide MathExpr MathExpr  -- division
               | Mult  MathExpr MathExpr   -- multiply two MathExpr's 
               | Expon MathExpr MathExpr   -- set first MathExpr to the second MathExpr power
               | Add MathExpr MathExpr
               | Sub MathExpr MathExpr
               | Neg MathExpr
               deriving Show

data Expr      = Var String | Const Integer | Op Opr [Expr] deriving (Eq)
data Opr       = Multiplication | Division | Addition | Subtraction
               | Exponentiation | Factorial | Negate deriving (Eq,Show)

data Law        = Law {lawName :: String, lawEq :: Equation}
type Equation   = (Expr,Expr)

data IneqLaw    = IneqLaw {ineqName :: String, ineqEq :: Implies}
data Proof     = Proof Expr [ProofStep] deriving Show
type ProofStep = (String, Expr)
data IneqProof = IneqProof Inequality [IneqProofStep]
type IneqProofStep = (String, Inequality)
type Inequality = (Expr,Ineq,Expr)
data Ineq       = GThan | GEq | EqEq deriving (Eq)
type Implies    = (Inequality,Inequality)

prec :: Opr -> Int
prec Factorial = 3
prec Exponentiation = 3
prec Negate = 3
prec Multiplication = 2
prec Division = 2
prec Addition = 1
prec Subtraction = 1

symb :: Opr -> String
symb Multiplication = " \\cdot "
symb Division = " / "
symb Addition = " + "
symb Subtraction = " - "
symb Factorial = "!"
symb Exponentiation = "^"
symb Negate = "-"


instance Show Law where 
  showsPrec _ (Law name (e1,e2))
    = showString name . 
      showString ": " . 
      shows e1 .
      showString " = " . 
      shows e2

instance Show IneqLaw where 
  showsPrec _ (IneqLaw name ((e11,i1,e12),(e21,i2,e22)))
    = showString name . 
      showString ": " . 
      shows e11 . shows i1 . shows e12 .
      showString " \\Rightarrow " . 
      shows e21 . shows i2 . shows e22

instance Show Ineq where
  showsPrec _ GThan = showString " > "
  showsPrec _ GEq   = showString " ≥ "
  showsPrec _ EqEq  = showString " = "

instance Show Expr where
  showsPrec p (Var s) = showString (show s)
  showsPrec p (Const n) = showString (show n)
  showsPrec p (Op o [e1]) = case o of
    Factorial -> showParens (p>q) (showsPrec q e1 . showString (symb Factorial))
    Negate -> showParens (p>q) (showString (symb Negate) . showsPrec q e1)
    where q = prec o
  showsPrec p (Op o [e1,e2]) = showParens (p>q) (showsPrec q e1 . showString (symb o) . showsPrec (q+1) e2)
      where q = prec o

showParens :: Bool -> (String -> String) -> (String -> String)
showParens True s = showChar '(' . s . showChar ')'
showParens False s = s

{- laws to be used in our proofs -}

-- some of these are duplicated, ex: a * 0 = 0 and 0 * a = 0
lawList :: [String]
lawList = [ "Commutative of Addition: a + b = b + a"
          , "Additive Identity: a + 0 = a"
          , "Additive Identity: 0 + a = a"
          , "Additive Inverse: a - a = 0"
          , "Additive Association: a + (b + c) = (a + b) + c"
          , "Multiplicative Identity: a * 1 = a"
          , "Multiplicative Identity: 1 * a = a"
          , "Multiplicative Association: a * (b * c) = (a * b) * c"
          , "Multiplication Times 0: 0 * a = 0"
          , "Multiplication Times 0: a * 0 = 0"
          , "Dividing Zero: 0 / a = 0"
          , "Multiplicative Inverse: a / a = 1"
          , "Distributive Law: a - (b + c) = a - b - c"
          , "Distributive Law: a - (b - c) = a - b + c" ]

ineqLawList :: [String]
ineqLawList = [ --"Multiplication for x > 0: x * y > x * z \\Rightarrow y > z"    -- hold off for now
              --, "Multiplication for x > 0: y * x > z * x \\Rightarrow y > z"    -- hold off for now
                "Multiplication for x ≥ 0: y > z \\Rightarrow x * y ≥ x * z"      -- done
              , "Multiplication for x > 0: y > z \\Rightarrow x * y > x * z"      -- done
              , "Multiplication for x > 0: y > z \\Rightarrow y * x > z * x"      -- done
              , "Multiplication for x ≥ 0: y > z \\Rightarrow y * x ≥ z * x"      -- done
              , "Addition: x > y \\Rightarrow x + z > y + z"
              , "Division for x > 0 and z > 0: x < y \\Rightarrow z / x > z / y"
              , "Division for x > 0 and z > 0: x > y \\Rightarrow z / x < z / y"
              , "Division for x > 0 and z ≥ 0: x < y \\Rightarrow z / x ≥ z / y"
              , "Division for x > 0 and z ≥ 0: x > y \\Rightarrow z / x ≤ z / y"
              , "Numbers: 1 > 0" -- idk what to do about this last one but it's needed
              ]

lawBois = stringsToLaws lawList
stringsToLaws :: [String] -> [Law]
stringsToLaws l = catMaybes $ map convert l
  where convert v = case parse parseLaw "" v of
                 Left _ -> Nothing
                 Right v -> Just v

digit :: Parser Integer
digit = do c <- satisfy inRange
           return (toInteger (charToInteger c))
   where charToInteger c = fromEnum c - fromEnum '0'
         inRange c = let r = charToInteger c
                     in r >= 0 && r < 10

-- maybe add support for uppercase letter later
var :: Parser Char
var = do c <- satisfy inRange
         return c
   where charToInteger c = fromEnum c - fromEnum 'a'
         inRange c = let r = charToInteger c
                     in r >= 0 && r < 26

-- maybe add support for uppercase letter later
parseVar :: Parser MathExpr
parseVar = do s <- some var
              return (MathVar s)

spaces :: Parser ()
spaces = many spc >> return ()
 where spc = string " " <|> string "\t" <|> string "\n" <|> string "\r"
             <|> string "\\ " <|> string "~"

-- parse a given specific string, accounting for spaces 
symbol :: String -> Parser String
symbol = lexeme . string

-- parse some lexeme, accounting for spaces
lexeme :: Parser a -> Parser a
lexeme x = spaces *> x <* spaces

-- parse something between (...) 
parens :: Parser a -> Parser a          
parens = between (symbol "(") (symbol ")")

-- parses some integer number alone into MathExpr
parseConstant :: Parser MathExpr
parseConstant = do
  n <- lexeme L.decimal
  return (MathConst n)

-- operator table for use with makeExprParser 
operatorTable :: [[Operator Parser MathExpr]]
operatorTable =
  [ [postfix "!" Fact],
    [prefix "-" Neg],
    [binary "^" Expon] ,
    [binary "\\cdot" Mult, binary "*" Mult, binary "/" Divide] ,
    [binary "+" Add, binary "-" Sub]
  ]

-- helper function for generating an binary infix operator
-- like multiplication or exponentiation
-- based on documentation for Control.Monad.Combinators.Expr
binary :: String -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ symbol name)

-- helper function for postfix operators like factorial
-- also from Control.Monad.Combinators.Expr documentation 
postfix :: String -> (a -> a) -> Operator Parser a
postfix name f = Postfix (f <$ symbol name)

prefix :: String -> (a -> a) -> Operator Parser a
prefix  name f = Prefix (f <$ symbol name)

-- parses a term (some expression in brackets/parens, a constant alone, or a binom/frac)
parseTerm :: Parser MathExpr
parseTerm = (parens parseMathExpr <|> parseConstant <|> parseVar) <* spaces

-- parse a full expression (using makeExprParser)
parseMathExpr :: Parser MathExpr
parseMathExpr = spaces *> makeExprParser parseTerm operatorTable <* spaces

parseExpr :: Parser Expr
parseExpr = do mex <- parseMathExpr
               return (mathToExpr mex)

mathToExpr :: MathExpr -> Expr
mathToExpr (MathVar s) = Var s
mathToExpr (MathConst n) = Const n
mathToExpr (Fact a) = Op Factorial [mathToExpr a]
mathToExpr (Divide a1 a2) = Op Division [mathToExpr a1, mathToExpr a2]
mathToExpr (Mult a1 a2) = Op Multiplication [mathToExpr a1, mathToExpr a2]
mathToExpr (Expon a1 a2) = Op Exponentiation [mathToExpr a1, mathToExpr a2]
mathToExpr (Add a1 a2) = Op Addition [mathToExpr a1, mathToExpr a2]
mathToExpr (Sub a1 a2) = Op Subtraction [mathToExpr a1, mathToExpr a2]
mathToExpr (Neg a) = Op Negate [mathToExpr a]

parseLaw :: Parser Law
parseLaw = do lname <- parseUntil ':'
              lhs     <- parseExpr
              _       <- string "="
              rhs     <- parseExpr
              return (Law lname (lhs,rhs))

parseUntil :: Char -> Parser String
parseUntil c = (do _ <- satisfy (== c)
                   return  []) <|>
               (do c1 <- satisfy (const True)
                   rmd <- parseUntil c
                   return (c1:rmd)
               )

-- law1 = Law "Assoc" (Op Addition [Op Addition [Var "X",Var "Y"],Var "Z"], Op Addition [Var "X", Op Addition [Var "Y", Var "Z"]])

-- generateRandEx :: Int -> ChoiceTree Expr
-- generateRandEx = undefined

getDerivation :: [Law] -> Ineq -> Expr -> Proof
getDerivation laws ineq e = Proof e (multiSteps e)
  where multiSteps e'
          = case [ (lawName law, res)
                 | law <- laws
                 , res <- getStep2 laws (lawEq law) ineq e'
                 ] of
            [] -> []
            ((nm,e''):_) -> (nm,e'') : multiSteps e''

getStep :: Equation -> Expr -> [Expr]
getStep (lhs, rhs) expr
  = case matchE lhs expr of
      Nothing -> recurse expr
      Just subst -> [apply subst rhs]
  where recurse (Var _) = []
        recurse (Const _) = []
        recurse (Op o [e1,e2])
          = [Op o [e1', e2] | e1' <- getStep (lhs,rhs) e1] ++
            [Op o [e1, e2'] | e2' <- getStep (lhs,rhs) e2]
        recurse (Op o [e1])
          = [Op o [e1'] | e1' <- getStep (lhs,rhs) e1]

getStep2 :: [Law] -> (Expr, Expr) -> Ineq -> Expr -> [Expr]
getStep2 laws (lhs, rhs) ineq e
  = case matchE lhs e of
      Nothing -> recurse e
      Just subst -> [apply subst rhs]
  where recurse (Var _) = []
        recurse (Const _) = []
        recurse (Op o [e1,e2]) 
          = [Op o [e1', e2] | e1' <- getStep (lhs,rhs) e1, snd (checkFunc o) e2] ++
            [Op o [e1, e2'] | e2' <- getStep (lhs,rhs) e2, fst (checkFunc o) e1]
        recurse (Op o [e1])
          = [Op o [e1'] | e1' <- getStep (lhs,rhs) e1]

        --"Multiplication for x > 0: y > z \\Rightarrow x * y > x * z"
        isPositive e1 = provesGEQ 1 (getDerivation laws ineq e1)
        isNonNeg e1   = provesGEQ 0 (getDerivation laws ineq e1)
        provesGEQ b (Proof e1 steps) = go b e1 steps
              where go b' (Const n) _ | n >= b'     = True
                                     | otherwise = False
                    --go b' (Op Negation [e]) _ = False
                    go b' _ ((_ , e2):ss) = go b' e2 ss --b-1 with strict inequalities
                    go _ _ [] = False

        checkFunc Multiplication = case ineq of 
                                    EqEq  -> (const True, const True)
                                    GThan -> (isPositive, isPositive)
                                    GEq   -> (isNonNeg, isNonNeg)
        checkFunc Addition       = (const True, const True)
        checkFunc Division       = (const False, snd (checkFunc Multiplication))
        checkFunc Subtraction    = (const False, const True)
        
-- I think we will need to change this function
type Substitution = [(String, Expr)]
matchE :: Expr -> Expr -> Maybe Substitution
matchE (Var nm) expr = Just [(nm,expr)]
matchE (Const i) (Const j) | i == j = Just []
matchE (Const _) _ = Nothing
matchE (Op o1 [e1,e2]) (Op o2 [e3,e4]) | o1 == o2
 = case (matchE e1 e3, matchE e2 e4) of
    (Just s1, Just s2) -> combineTwoSubsts s1 s2
    _ -> Nothing
matchE (Op o1 [e1]) (Op o2 [e2]) | o1 == o2
 = matchE e1 e2
matchE (Op _ _) _ = Nothing

combineTwoSubsts :: Substitution -> Substitution -> Maybe Substitution
combineTwoSubsts s1 s2
  = case and [v1 == v2 | (nm1,v1) <- s1, (nm2,v2) <- s2, nm1 == nm2] of
     True -> Just (s1 ++ s2)
     False -> Nothing

apply :: Substitution -> Expr -> Expr
apply subst (Var nm) = lookupInSubst nm subst
apply _ (Const i) = Const i
apply subst (Op o [e]) = Op o [apply subst e]
apply subst (Op o [e1,e2]) = Op o [(apply subst e1),(apply subst e2)]

lookupInSubst :: String -> [(String, p)] -> p
lookupInSubst nm ((nm',v):rm)
 | nm == nm' = v
 | otherwise = lookupInSubst nm rm
lookupInSubst _ [] = error "Substitution was not complete, or free variables existed in the rhs of some equality"

-- only supports monotonically increasing functions of one variable
-- i realize those are some pretty narrow restrictions
-- can expand the function if necessary
-- or we can just carefully generate expressions that fit these params
evaluate :: Expr -> Expr -> Maybe (Ineq,Integer)
evaluate one two = 
  let s = seeWhatWorks one two
  in findIneq (findSmallest s) s
  where
    seeWhatWorks left right = [ (leftVal,rightVal,tryNum)
                              | tryNum <- [0..100]
                              , let (l,r) = sub (left,right) "n" tryNum
                              , let leftVal = compute l
                              , let rightVal = compute r
                              , isJust leftVal && isJust rightVal
                              , leftVal >= rightVal]
    findSmallest l = case last l of
                    (_,_,100) -> Just (last (zipWith (\(_,_,x) y -> x - y) l [0..100]) + 1)
                    (_,_,_) -> Nothing

expr1 = Op Addition [Op Multiplication [Const 3,Var "n"], Const 6]
expr2 = Op Exponentiation [Var "n", Const 2]
expr3 = Op Factorial [Var "n"]

findIneq :: Maybe Integer -> [(Maybe Integer,Maybe Integer,Integer)] -> Maybe (Ineq,Integer)
findIneq Nothing _ = Nothing
findIneq _ [] = Nothing
findIneq (Just v) ((l,r,n):xs) | n == v = case (l>r) of
                                            True -> Just (GThan,n)
                                            False -> Just (GEq,n)
                               | n == v + 1 = case (l>r) of
                                                True -> Just (GThan,n)
                                                False -> Just (GEq,n)
                               | otherwise = findIneq (Just v) xs

sub :: (Expr,Expr) -> String -> Integer -> (Expr,Expr)
sub (l,r) v n = (go l v n, go r v n)
  where go expr val num
          = case expr of
              Const someNum -> Const someNum
              Var val -> Const n
              Var otherVal -> Var otherVal -- idk what to do about this particular line
              Op o [e1] -> Op o [go e1 val num]
              Op o [e1,e2] -> Op o [go e1 val num, go e2 val num]

-- Does only integer math for now
-- Would need to alter our datatype to do work with rationals/floats
compute :: Expr -> Maybe Integer
compute (Const c) = Just c
compute (Var _) = Just 1 -- ??????
compute (Op o [e1]) = case (compute e1, o) of
                          (Nothing,_) -> Nothing
                          (Just eval1,Factorial) -> evalFac eval1
                          (Just eval1, Negate) -> Just (-1 * eval1)
compute (Op o [e1,e2]) = case (compute e1, compute e2, o) of
                          (Nothing,_,_) -> Nothing
                          (Just eval1, Nothing,_) -> Nothing
                          (Just eval1, Just eval2, Addition)-> Just (eval1 + eval2)
                          (Just eval1, Just eval2, Subtraction) -> Just (eval1 - eval2)
                          (Just eval1, Just eval2, Multiplication) -> Just (eval1 * eval2)
                          (Just eval1, Just eval2, Division) -> case eval2 of
                                                                      0 -> Nothing
                                                                      _ -> Just (eval1 `div` eval2)
                          (Just eval1, Just eval2, Exponentiation) -> Just (eval1 ^ eval2)

evalFac :: Integer -> Maybe Integer
evalFac n = case (n>=0) of
              False -> Nothing
              True -> Just (realFac n)
  where realFac 0 = 1
        realFac x = x * realFac (x - 1)
              



-- {- displaying the proofs -}

-- permutations :: Int -> ChoiceTree [Int]
-- permutations 0 = Node []
-- permutations n -- ChoiceTree is a Monad now! I've also derived "Show", so you can more easily check this out in GHCI.
--  = do i <- nodes [0..n-1]
--       rm <- permutations (n-1)
--       return (i : map (\v -> if v >= i then v+1 else v) rm)

-- breakUnderscore :: String -> [String]
-- breakUnderscore s
--   =  case dropWhile (=='_') s of
--        "" -> []
--        s' -> w : breakUnderscore s''
--              where (w, s'') = break (=='_') s'

-- ineqProofExercise :: ExerciseType
-- ineqProofExercise = exerciseType "ineqProof" "L?.???" "Logic: Inequality"
--                       [permutations 5] genProof simpleFeedback
--                where simpleFeedback sol rsp pr
--                       = case Map.lookup "proof" rsp of
--                           Just str
--                             -> if map ((sol !!) . read ) (breakUnderscore str) == [0..4]
--                                then markCorrect pr
--                                else markWrong pr{prFeedback=[FText "You answered: ",FText str]}
--                           Nothing -> error "Client response is missing 'proof' field"

-- -- data Proof     = Proof Expr [ProofStep] deriving Show
-- -- type ProofStep = (String, Expr)

-- genProof :: [Int] -> Exercise -> Exercise
-- genProof order def 
--  = def{ eQuestion = [ FText $"Can you put the following proof in the right order?"
--                     , FIndented 1 [FMath "(n + 1)"]
--                     , FReorder "proof"
--                         (map ([step1,step2,step3,step4,step5] !!) order)
--                     ]
--       , eBroughtBy = ["Kyle Bensink and Lucas Boebel"] }
-- -- getDerivation :: [Law] -> Ineq -> Expr -> Proof

--       where --hardcoded example
--         step1 = [ FMath "=", FText "{ definition of factorial }"
--                 , FIndented 1 [FMath "(n + 1)! ≥ (n + 1) \\cdot (n)!"] ]
--         step2 = [ FMath "=", FText "{ Given x > 0: n + 1 > 0, n! > n }"
--                 , FIndented 1 [FMath "(n + 1)! > (n + 1) \\cdot (n)!"] ]
--         step3 = [ FMath "=", FText "{ n > 0, n + 1 > 1 }"
--                 , FIndented 1 [FMath "(n + 1)! > (n + 1) \\cdot n"] ]
--         step4 = [ FMath "=", FText "{ n > 0, n + 1 > 1 }"
--                 , FIndented 1 [FMath "(n + 1)! > 1 * n"] ]
--         step5 = [ FMath "=", FText "{ Multiplication by 1 }"
--                 , FIndented 1 [FMath "(n + 1)! > n"] ]



