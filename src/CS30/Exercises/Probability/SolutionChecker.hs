{-# LANGUAGE BlockArguments #-}
module CS30.Exercises.Probability.SolutionChecker where
import           CS30.Data
import           CS30.Exercises.Util
import           Control.Monad.Combinators.Expr
import qualified Data.Map as Map
import           Data.Ratio
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char

type Parser = Parsec Void String

data NumericExpression = Const Rational
                        | BinOp NumbericOper NumericExpression NumericExpression deriving (Show)-- stands for a + b where a and b are NumericExpression

data NumbericOper = Multiplication | Division | Addition | Subtraction | Exponentiation deriving (Show)

digit :: Parser Integer
digit = do a <- satisfy satiDigit
           return (toInteger ((fromEnum a - fromEnum '0')))

satiDigit :: Enum a => a -> Bool
satiDigit a = num >= 0 && num < 10
            where num = fromEnum a - fromEnum '0'

-- 0 \cdot 667
-- 66.7%
-- \frac{1}{3}
-- parser to parse the user solutions
numeric_value_parser :: Parser NumericExpression
numeric_value_parser = lt_spaces *> makeExprParser digits oprs <* lt_spaces
 where digits = (do fstPart <- some digit
                    rmdr <- (string "." *> many digit) <|> return []
                    lt_spaces
                    perc <- (return True <* string "\\%") <|> return False
                    lt_spaces
                    -- 12.5: wholePart [1,2] rmdr [5]
                    -- 12. : wholePart [1,2] rmdr []
                    -- 12  : wholePart [1,2] rmdr []
                    -- .5
                    let wholeNr = parseDec fstPart % 1
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
                (do _ <- string "\\frac"
                    t1 <- digits
                    t2 <- digits
                    lt_spaces
                    return (BinOp Division t1 t2))
                <|>
                (do _ <- string "\\left"
                    _ <- openBrac
                    lt_spaces
                    e <- numeric_value_parser
                    _ <- string "\\right"
                    _ <- closeBrac
                    lt_spaces
                    return e
                    )
                <|>
                (do _ <- openBrac
                    lt_spaces
                    e <- numeric_value_parser
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
       oprs =  [ [ infixL "^" Exponentiation] -- negate, exponentiation
                , [ infixL "\\cdot" Multiplication -- for LaTeX
                    , infixL "*" Multiplication -- for ASCII
                    , infixL "/" Division] -- multiplication, division
                , [ infixL "-" Subtraction
                    , infixL "+" Addition] -- minus and addition
                ]
       infixL str opr = InfixL (return (BinOp opr) <* string str <* lt_spaces)

evalRational :: NumericExpression -> Maybe Rational
evalRational (Const r) = return r
evalRational (BinOp op e1 e2)
      = do e1v <- evalRational e1
           e2v <- evalRational e2
           opToFunction op e1v e2v

opToFunction :: NumbericOper -> Rational -> Rational -> Maybe Rational
opToFunction Multiplication r1 r2 = return (r1 * r2)
opToFunction Division r1 r2 = if (r2 == 0) then Nothing else return (r1/r2)
opToFunction Addition r1 r2 = return (r1+r2)
opToFunction Subtraction r1 r2 = return (r1-r2)
opToFunction Exponentiation r1 r2
                     | (denominator r2 == 1) = return (r1 ^^ (numerator r2))
                     | otherwise = Nothing

genFeedback :: ([Field], Rational)
              -> Map.Map String String
              -> ProblemResponse
              -> ProblemResponse
genFeedback (_, solution)  mStrs rsp = reTime $
                  case Map.lookup "answer" mStrs of
                      Nothing -> error "Answer field expected"
                      Just v -> case runParser numeric_value_parser "" v of
                                  Left e -> markWrong $ rsp{prFeedback = [FText "You entered " , FMath $ show v, FText (", parse error" ++ errorBundlePretty e)]}
                                  Right userAnswer -> case evalRational userAnswer of
                                                        Nothing -> markWrong $ rsp{prFeedback = [FText "Sorry! You entered ", FMath $ show userAnswer, FText ", which we couldn't evaluate (division by zero?)"]}
                                                        Just userSolution -> if userSolution == solution then
                                                                                markCorrect $
                                                                                    rsp{prFeedback = [FText "Congratulations! You entered ", FMath $ show userAnswer, FText ", the right answer is ", FMath$ show solution]}
                                                                            else markWrong $
                                                                                    rsp{prFeedback = [FText "Sorry! You entered ", FMath $ show userAnswer, FText ", the answer is wrong"]}