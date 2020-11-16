{-# LANGUAGE BlockArguments #-}
module CS30.Exercises.ProofProbability where
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           CS30.Exercises.Probability.SolutionChecker
import CS30.Exercises.Data


data FracExpr 
    = FConst Rational -- the constants 0 and 1
    | FVar Char -- basic evert : A, B 
    | FExpr FracExpr FracExpr -- ^ probability expression like Pr[A|B].
    | FBin MathExpr FracExpr FracExpr -- ^ multiplication
    | AndEvent FracExpr FracExpr  -- ‘and’ \wedge and ‘or’ \vee  
    | OrEvent FracExpr FracExpr 
    | NegaEvent FracExpr  -- event negation \neg
    deriving (Show, Eq)

data Law = Law {lawName :: String, lawEq :: Equation} deriving Show
type Equation = (FracExpr,FracExpr)

-- laws given in the Final Assignmentss
deMorgan1 :: String
deMorgan1 =  "DeMorgan: \\neg (A \\wedge B) = \\neg A \\vee \\neg B"
deMorgan2 :: String
deMorgan2 = "DeMorgan: \\neg (A \\vee B) = \\neg A \\wedge \\neg B"
dbNegat :: String
dbNegat = "Double negation: \\neg (\\neg A) = A"
omegaElimiW :: String
omegaElimiW = "Omega elimination: \\Omega \\wedge A = A"
omegaElimiV :: String
omegaElimiV = "Omega elimination: \\Omega \\vee A = \\Omega"
emptyElimiW :: String
emptyElimiW = "Empty-set elimination: \\emptyset \\vee A = A"
emptyElimiV :: String
emptyElimiV = "Empty-set elimination: \\emptyset \\wedge A = \\emptyset"
negaProba :: String
negaProba = "Negation in probability: Pr[\\neg A] = 1 - Pr[A]"
difference :: String
difference = "Difference: Pr[A \\wedge \\neg B] = Pr[A] - Pr[A \\wedge B]"

-- x - y = x + (nagate y)
-- lhs : x - y; rhs : x + (nagate y)
parseLaws :: Parser Law
parseLaws = do {lawname <- parseUntil ':';
                lt_spaces;
                lhs <- fractional_parser;
                _ <- string "=";
                rhs <- fractional_parser;
                return (Law lawname (lhs,rhs))}

parseUntil :: Char -> Parser String 
parseUntil c
  = (do _ <- satisfy (== c)
        return []) 
    <|> 
    (do accum <- satisfy (const True) 
        rmd <- parseUntil c 
        return (accum:rmd) 
        )

laws :: [String] 
laws = [deMorgan1, deMorgan2] --, dbNegat, omegaElimiW, omegaElimiV, emptyElimiW, emptyElimiV]

parsedLaws :: [Law]
parsedLaws = Prelude.map strToLaw laws

strToLaw :: String -> Law 
strToLaw law =  case parse parseLaws "" law of
                Right st -> st
                Left _ -> error "Error parsing one of the internal laws"


data Proof = Proof FracExpr [FracStep] deriving (Show, Eq)
type FracStep = (String, FracExpr)

-- generate two event expression from a set of base events
genEveExper :: FracExpr -> (FracExpr, FracExpr)
genEveExper = undefined

-- find proof in terms of laws
-- check lhs and then change to rhs if the lhs are same
getDerivation :: [Law] -> FracExpr -> Proof
getDerivation lawss e 
    = Proof e (multiSteps e)
    where multiSteps e' = case [(lawName law , res)
                    | law <- lawss
                    , res <- getStep (lawEq law) e'] of  -- match rhs 
                [] -> [] 
                ((nm, e''):_) -> (nm, e'') : multiSteps e''

-- x - y = x + (nagate y)
-- lhs : x - y; rhs : x + (nagate y)
-- expr : (5-3) - 1
-- subst to get: x = 5-3, y = 1
getStep :: Equation -> FracExpr -> [FracExpr]
getStep (lhs, rhs) expr 
    = case matchE lhs expr of
        Nothing -> recurse expr 
        Just subst -> [apply subst rhs]
    where 
        recurse (FBin op e1 e2)   = [FBin op e1' e2 | e1' <- getStep(lhs, rhs) e1] ++  
                                     [FBin op e1 e2' | e2' <- getStep(lhs, rhs) e2]
        recurse (FExpr e1 e2)     = [FExpr e1' e2| e1' <- getStep (lhs, rhs) e1] ++
                                     [FExpr e1 e2'| e2' <- getStep (lhs, rhs) e2]
        recurse (AndEvent e1 e2)  = [AndEvent e1' e2| e1' <- getStep (lhs, rhs) e1] ++
                                     [AndEvent e1 e2'| e2' <- getStep (lhs, rhs) e2]
        recurse (OrEvent e1 e2)   = [OrEvent e1' e2| e1' <- getStep (lhs, rhs) e1] ++
                                     [OrEvent e1 e2'| e2' <- getStep (lhs, rhs) e2]
        recurse (NegaEvent e1) = [NegaEvent e1'| e1' <- getStep (lhs, rhs) e1] 

        recurse _ = []

-- check the lhs of laws
-- create a substitution list 
type Substitution = [(Char, FracExpr)]
matchE :: FracExpr -> FracExpr -> Maybe Substitution
matchE (FVar nm) expr                     = Just [(nm, expr)]
matchE (FConst i) (FConst j)              | i == j = Just []
matchE (FConst _) _                       = Nothing
matchE (FExpr e1 e2) (FExpr e3 e4)        = matchHelper (matchE e1 e3)(matchE e2 e4) -- P(A|B)P(C|D)
matchE (FBin op e1 e2) (FBin op1 e3 e4)   |op == op1 = matchHelper (matchE e1 e3)(matchE e2 e4)
matchE (FBin _ _ _) _                     = Nothing
matchE (AndEvent e1 e2) (AndEvent e3 e4)  = matchHelper (matchE e1 e3)(matchE e2 e4) 
matchE (OrEvent e1 e2) (OrEvent e3 e4)    = matchHelper (matchE e1 e3)(matchE e2 e4) 
matchE (NegaEvent e1) (NegaEvent e3)      = matchE e1 e3
matchE _ _                                = Nothing


matchHelper :: Maybe Substitution -> Maybe Substitution -> Maybe Substitution
matchHelper sub1 sub2 = case (sub1, sub2) 
                        of (Just s1, Just s2) -> combineTwoSubsts s1 s2
                           _ -> Nothing

combineTwoSubsts ::  Substitution -> Substitution -> Maybe Substitution
combineTwoSubsts  s1 s2 
    = case and [v1 == v2 | (nm1,v1) <- s1, (nm2,v2) <- s2, nm1 == nm2] of    
        True -> Just (s1 ++ s2)    
        False -> Nothing

-- change the lhs
apply :: Substitution -> FracExpr -> FracExpr
apply subst (FVar nm)           = lookupInSubst nm subst 
apply _ (FConst i)          = FConst i
apply subst (FExpr e1 e2)       = FExpr (apply subst e1) (apply subst e2) 
apply subst (FBin op e1 e2)     = FBin op (apply subst e1) (apply subst e2) 
apply subst (AndEvent e1 e2)    = AndEvent (apply subst e1) (apply subst e2) 
apply subst (OrEvent e1 e2)     = OrEvent (apply subst e1) (apply subst e2)  
apply subst (NegaEvent e1)      = NegaEvent (apply subst e1) 

lookupInSubst :: Char -> [(Char, p)] -> p
lookupInSubst nm ((nm', v) : rm)
    | nm == nm' = v 
    | otherwise = lookupInSubst nm rm 
lookupInSubst _[] = error "Substitution was not complete, or free variables existed in the rhs of some equality"

-- parser single char for basic event
basEvent_parser :: Parser FracExpr
basEvent_parser = do {lt_spaces; 
                    c <- satisfy isLetterforEvent;
                    _ <- lt_spaces;
                    return (FVar c)}
                where isLetterforEvent v = elem v ['A'..'Z']
               
fractional_parser :: Parser FracExpr
fractional_parser = do {lt_spaces;
                    do {r <- (string "0"*> return 0) <|> (string "1"*> return 1);
                        lt_spaces;
                        return (FConst r)}
                    <|>
                    do {string "Pr[";
                        e1 <- fractional_parser;
                        string "|";
                        e2 <- fractional_parser;
                        string "]";
                        lt_spaces;
                        return (FExpr e1 e2)
                    }
                    <|>
                    do {
                        string "(";
                        e1 <- fractional_parser;
                        string ")";
                        lt_spaces;
                        return (e1)
                    }
                    <|>
                    do {
                        e1 <- fractional_parser;
                        string "\\wedge";
                        e2 <- fractional_parser;
                    
                        lt_spaces;
                        return (AndEvent e1 e2)
                    } 
                    <|>
                    do {
                        e1 <- fractional_parser;
                        string "\\vee";
                        e2 <- fractional_parser;
                        
                        lt_spaces;
                        return (OrEvent e1 e2)
                    }
                    <|>
                    do {
                        string "\\neg";
                        e1 <- fractional_parser;
                        lt_spaces;
                        return (NegaEvent e1)
                    }
                    <|>
                    do {
                        e1 <- basEvent_parser;
                        return (e1)
                    }
                    } 



-- codes copied from Cardinality.hs
data MathExpr 
    = Const Rational 
    | Divide MathExpr MathExpr  -- division
    | Mult  MathExpr MathExpr  -- multiply two MathExpr's 
    deriving (Show, Eq)

-- ^here lets us parse things like (3)(3) = 3*3
{- operatorTable :: [[Operator Parser MathExpr]]
operatorTable = [[binary "\\cdot" Mult, binary "" Mult]]

binary :: String -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ symbol name)

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = L.space spaces empty empty 

spaces :: Parser ()
spaces = some spc >> return ()
 where spc = string " " <|> string "\t" <|> string "\n" <|> string "\r"
             <|> string "\\ " <|> string "~"-} 


-- generate problems 
generateRandEx :: Int -> ChoiceTree FracExpr
generateRandEx = undefined

example1, example2, example3, example4, example5, example6 :: FracExpr
example1 = FConst 0
example2 = AndEvent (FVar 'A') (FVar 'B')
example3 = OrEvent (FVar 'A') (FVar 'B')
example4 = NegaEvent (FVar 'A')
example5 = FVar 'C'
example6 = NegaEvent (AndEvent (FVar 'A') (FVar 'B'))