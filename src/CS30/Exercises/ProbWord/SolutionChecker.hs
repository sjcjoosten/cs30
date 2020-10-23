{-# LANGUAGE OverloadedStrings #-}
module CS30.Exercises.ProbWord.SolutionChecker (probFeedback, probExpectFeedback) where
import           CS30.Data
import           CS30.Exercises.Util
import qualified Data.Map as Map
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import Control.Monad.Combinators.Expr
import GHC.Real -- for (%)
import Numeric ( showFFloat ) 

type Parser = Parsec Void String
data RationalExpr
      = Const Rational
      | BinOp RationalOpr RationalExpr RationalExpr
      | Negate RationalExpr deriving (Show)
data RationalOpr
       = Multiplication | Division | Addition | Subtraction
       | Exponentiation deriving (Show)

-- 3+5
-- 3\cdot 5
-- -3-5
-- \frac{3}{5}
-- 3^5
digit :: Parser Integer
digit = do c <- satisfy inRange
           return (toInteger (charToInteger c))
   where charToInteger c = fromEnum c - fromEnum '0'
         inRange c = let r = charToInteger c
                     in r >= 0 && r < 10
rationalExpr :: Parser RationalExpr
rationalExpr = lt_spaces *> makeExprParser termP oprs <* lt_spaces
  where termP = (do wholePart <- some digit
                    rmdr <- (string "." *> many digit) <|> return []
                    lt_spaces
                    perc <- (return True <* string "\\%") <|> return False
                    lt_spaces
                    -- 12.5: wholePart [1,2] rmdr [5]
                    -- 12. : wholePart [1,2] rmdr []
                    -- 12  : wholePart [1,2] rmdr []
                    -- .5
                    let wholeNr = parseDec wholePart % 1
                    let decNr = parseDec rmdr % 10^(length rmdr)
                    let ans = wholeNr + decNr
                    return (Const (if perc then ans / 100 else ans)))
                <|>
                (do _ <- string "."
                    rmdr <- some digit
                    lt_spaces
                    perc <- (return True <* string "\\%") <|> return False
                    lt_spaces
                    let ans = parseDec rmdr % 10^(length rmdr)
                    return (Const (if perc then ans / 100 else ans)))
                <|>
                (do _ <- "\\frac"
                    t1 <- termP
                    t2 <- termP
                    lt_spaces
                    return (BinOp Division t1 t2))
                <|>
                (do _ <- "\\left"
                    _ <- openBrac
                    lt_spaces
                    e <- rationalExpr
                    _ <- "\\right"
                    _ <- closeBrac
                    lt_spaces
                    return e
                    )
                <|>
                (do _ <- openBrac
                    lt_spaces
                    e <- rationalExpr
                    _ <- closeBrac
                    lt_spaces
                    return e
                    )
        openBrac = string "(" <|> string "{"
        closeBrac = string ")" <|> string "}"
        parseDec = foldl (\x y -> x*10 + y) 0
        -- Latex spaces parser, doesn't return the parsed string.
        lt_spaces :: Parser ()
        lt_spaces = ((string " " <|> string "\\t" <|> string "\\ ") *> lt_spaces)
                    <|> return ()
        oprs = [ [infixL "-" Subtraction, infixL "+" Addition] -- minus and addition
               , [ infixL "\\cdot" Multiplication -- for LaTeX
                  , infixL "*" Multiplication -- for ASCII
                  , infixL "/" Division] -- multiplication, division
               , [infixL "^" Exponentiation
                 ,Prefix (return Negate <* string "-" <* lt_spaces)] -- negate, exponentiation
               ]
        infixL str opr = InfixL (return (BinOp opr) <* string str <* lt_spaces)
evalRational :: RationalExpr -> Maybe Rational
evalRational (Const r) = return r
evalRational (BinOp op e1 e2)
  = do e1v <- evalRational e1
       e2v <- evalRational e2
       opToFunction op e1v e2v
evalRational (Negate e) = negate <$> evalRational e
opToFunction :: RationalOpr -> Rational -> Rational -> Maybe Rational
opToFunction Multiplication r1 r2 = return (r1 * r2)
opToFunction Division r1 r2 = if (r2 == 0) then Nothing else return (r1/r2)
opToFunction Addition r1 r2 = return (r1+r2)
opToFunction Subtraction r1 r2= return (r1-r2)
opToFunction Exponentiation r1 r2
 | (denominator r2 == 1) = return (r1 ^^ (numerator r2))
 | otherwise = Nothing

-- For Basic Probability problem, as we generally expect a fractional form answer, so Rational type is the way to go
probFeedback ::  ([Field], Rational) -> Map.Map String String -> ProblemResponse -> ProblemResponse
probFeedback (quer, sol) usr' defaultRsp
  = reTime$ case usr of 
              Nothing -> error "Server communication error: expecting a 'prob' field"
              Just v -> case runParser rationalExpr "" v of
                           Left e -> wrong{prFeedback=[FText$ "I didn't understand what you wrote. Here's a parse-error: "++errorBundlePretty e]}
                           Right expr -> case evalRational expr of
                                           Just userSol -> if userSol == sol then correct{prFeedback=rsp} else wrong{prFeedback=rsp++[FText$ ". You answered incorrectly: "]++rspwa}
  where 
  -- runParser ::
  -- Parsec e s a -> String -> s -> Either (ParseErrorBundle s e) a
    usr = Map.lookup "prob" usr' 
    dispRatio ratio = "\\frac{" ++ (show $ numerator ratio) ++ "}{" ++ (show $ denominator ratio) ++ "}"
    rsp = [FText $ "The answer for "]++quer++[FText " is ", FMath $ dispRatio sol]    
    rspwa = case usr of
            Nothing -> [FText "- ??? - (perhaps report this as a bug?)"]
            Just v -> [FMath v]
    wrong = markWrong defaultRsp
    correct = markCorrect defaultRsp

-- For Expected Value problem to display properly, we need a Double type for solution
probExpectFeedback :: ([Field], Double) -> Map.Map String String -> ProblemResponse -> ProblemResponse
probExpectFeedback (quer, sol) usr' defaultRsp
  = reTime$ case usr of 
              Nothing -> error "Server communication error: expecting a 'prob' field"
              Just v -> case runParser rationalExpr "" v of
                           Left e -> wrong{prFeedback=[FText$ "I didn't understand what you wrote. Here's a parse-error: "++errorBundlePretty e]}
                           Right expr -> case evalRational expr of
                                           Just userSol -> if realToFrac userSol ~== sol then correct{prFeedback=rsp} else wrong{prFeedback=rsp++[FText$ ". You answered incorrectly: "]++rspwa}
  where 
  -- runParser ::
  -- Parsec e s a -> String -> s -> Either (ParseErrorBundle s e) a
    (~==) a b = a - b < 1e-10 && a - b > (-(1e-10))
    usr = Map.lookup "prob" usr' 
    rsp = [FText $ "The answer for "]++quer++[FText " is ", FMath $ showFFloat (Just 2) sol ""]    
    rspwa = case usr of
            Nothing -> [FText "- ??? - (perhaps report this as a bug?)"]
            Just v -> [FMath v]
    wrong = markWrong defaultRsp
    correct = markCorrect defaultRsp
