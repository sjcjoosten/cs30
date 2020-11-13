module CS30.Exercises.GenerateExerViaProofs.GenerateProof where
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           CS30.Exercises.Probability.SolutionChecker
import Control.Monad.Combinators.Expr
import CS30.Exercises.GenerateExerViaProofs.ProofProbability
import qualified Data.Functor

-- datatypes for law
data Law = Law {lawName :: String, lawEq :: Equation} deriving Show
type Equation = (FracExpr,FracExpr)

-- laws given in the Final Assignments
rules :: [Law]
rules = map (takeRh . parse parseLaws "" ) 
        [deMorgan1, deMorgan2, dbNegat, omegaElimiW, omegaElimiV, negaProba, difference,
        condition, totalProb, bayesRule, incluExclu]

-- show if there is any error in parsing 
takeRh :: (Stream s, ShowErrorComponent e) => Either (ParseErrorBundle s e) p -> p
takeRh e = case e of (Right l) -> l
                     (Left err) -> error (errorBundlePretty err)

deMorgan1, deMorgan2, dbNegat, omegaElimiW, omegaElimiV, 
    negaProba, difference,emptyElimiW,emptyElimiV, 
    condition, totalProb, bayesRule, incluExclu :: String
deMorgan1 =  "DeMorgan: \\neg (A \\wedge B) = \\neg A \\vee \\neg B"
deMorgan2 = "DeMorgan: \\neg (A \\vee B) = \\neg A \\wedge \\neg B"
dbNegat = "Double negation: \\neg (\\neg A) = A"
omegaElimiW = "Omega elimination: \\Omega \\wedge A = A"
omegaElimiV = "Omega elimination: \\Omega \\vee A = \\Omega"
emptyElimiW = "Empty-set elimination: \\emptyset \\vee A = A"
emptyElimiV = "Empty-set elimination: \\emptyset \\wedge A = \\emptyset"
negaProba = "Negation in probability: Pr[\\neg A] = 1 - Pr[A]"
difference = "Difference: Pr[A \\wedge \\neg B] = Pr[A] - Pr[A \\wedge B]"
condition = "Definition of conditional probability: Pr[ A | B ] = Pr[A \\wedge B]\\cdot Pr[B]"
totalProb = "Law of total probability: Pr[ A ] = Pr[A | B]\\cdot Pr[B] + Pr[A | \\neg B]\\cdot Pr[\\neg B]"
bayesRule = "Bayes'rule: Pr[ A | B ] = \\frac{Pr[B | A]*Pr[A]}{Pr[B]}"
incluExclu = "Inclusion exclusion: Pr[A \\vee B] = Pr[A] + Pr[B] - Pr[A \\wedge B]"

-- parse Law's name and Law's equation
parseLaws :: Parser Law
parseLaws = do {lawname <- parseUntil ':';
                lt_spaces;
                lhs <- fractionalParser;
                _ <- string "=";
                lt_spaces;
                rhs <- fractionalParser;
                return (Law lawname (lhs,rhs))} -- lhs is the left hand side of "=".


parseUntil :: Char -> Parser String 
parseUntil c
  = (do _ <- satisfy (== c)
        return []) 
    <|> 
    (do accum <- satisfy (const True) 
        rmd <- parseUntil c 
        return (accum:rmd) 
        )


{- parsedLaws :: [Law]
parsedLaws = Prelude.map strToLaw laws-}

strToLaw :: String -> Law 
strToLaw law =  case parse parseLaws "" law of 
                Right st -> st


-- parse laws
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
-- check the lhs of laws
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


-- create a substitution list 
type Substitution = [(Char, FracExpr)]

-- match laws in terms of lhs
matchE :: FracExpr -> FracExpr -> Maybe Substitution
matchE (FVar nm) expr                     = Just [(nm, expr)]
matchE (FConst i) (FConst j)              | i == j = Just []
matchE (FConst _) _                       = Nothing
matchE (FExpr e1 e2) (FExpr e3 e4)        = matchHelper (matchE e1 e3)(matchE e2 e4) -- P(A|B)P(C|D)
matchE (FBin op e1 e2) (FBin op1 e3 e4)   |op == op1 = matchHelper (matchE e1 e3)(matchE e2 e4)
-- matchE (FBin _ _ _) _                     = Nothing
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
    = if and
       [v1 == v2 | (nm1, v1) <- s1, (nm2, v2) <- s2, nm1 == nm2] then
      Just (s1 ++ s2)
  else
      Nothing

-- change the lhs
apply :: Substitution -> FracExpr -> FracExpr
apply subst (FVar nm)           = lookupInSubst nm subst 
apply _ (FConst i)              = FConst i
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


-- parse basic event
basEventParser :: Parser FracExpr
basEventParser = do {lt_spaces; 
                    c <- satisfy isLetterforEvent;
                    _ <- lt_spaces;
                    return (FVar c)}
                where isLetterforEvent v = v `elem` ['A' .. 'Z']

-- parse "\Omege" and "\emptyset"
omeEmpParser :: Parser FracExpr
omeEmpParser = do {lt_spaces; 
                    _ <- string "\\Omega";
                    _ <- lt_spaces;
                    return Omega}
            <|> 
                do {lt_spaces; 
                    _ <- string "\\emptyset";
                    _ <- lt_spaces;
                    return EmptySet}

              
fractionalParser :: Parser FracExpr
fractionalParser = makeExprParser termParser operatorTable
             
termParser :: Parser FracExpr
termParser = do {r <- (string "0" Data.Functor.$> 0) <|> (string "1" Data.Functor.$> 1);
                lt_spaces;
                return (FConst r)}
            <|>
            omeEmpParser
            <|>
             do {_ <- string "(";
                lt_spaces;
                e1 <- fractionalParser;
                _ <- string ")";
                lt_spaces;
                return e1}
            <|>
             do {_ <- string "\\frac{";
                lt_spaces;
                e1 <- fractionalParser;
                lt_spaces;
                _ <- string "}";
                lt_spaces;
                _ <- string "{";
                e2 <- fractionalParser;
                lt_spaces;
                _ <- string "}";
                lt_spaces;
                return (FBin Frac e1 e2)}
            <|>
            do {_ <- string "Pr[";
                lt_spaces;
                e1 <- fractionalParser;
                _ <- string "]";
                lt_spaces;
                return (Prob e1)}
            <|> 
            basEventParser
            {- 
             <|>
             do {string "Pr[";
                lt_spaces;
                e1 <- fractional_parser;
                string "|";
                lt_spaces;
                e2 <- fractional_parser;
                string "]";
                lt_spaces;
                return (FExpr e1 e2)
                    } -}
        