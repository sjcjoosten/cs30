{-# LANGUAGE BlockArguments #-}
module CS30.Exercises.SolutionChecker where
import CS30.Data
import CS30.Exercises.Problems
import CS30.Exercises.Util
import CS30.Exercises.Data
import qualified Data.Map as Map
import           Data.Aeson.TH -- for deriveJSON
import Debug.Trace
-- import Data.Char (isNumber)
import Text.Megaparsec.Char
import Text.Megaparsec
import Data.Void
import Data.Ratio
import Control.Monad.Combinators.Expr
import Numeric ( showFFloat )

type Parser = Parsec Void String

data NumericExpression = Const Rational
                        | BinOp NumbericOper NumericExpression NumericExpression deriving (Show)-- stands for a + b where a and b are NumericExpression

data NumbericOper = Multiplication | Division | Addition | Subtraction | Exponentiation deriving (Show)

digit :: Parser Integer
digit = do a <- satisfy satiDigit
           return (toInteger ((fromEnum a - fromEnum '0')))

satiDigit a = num >= 0 && num < 10
            where num = fromEnum a - fromEnum '0'

-- 0 \cdot 667
-- 66.7%
-- \frac{1}{3}
numeric_value_parser :: Parser NumericExpression
numeric_value_parser = isSpace *> makeExprParser digits oprs <* isSpace
 where digits = (do fst_part <- many digit
                    rmd <- (Just <$> (string "." >> many digit)) <|> (return Nothing)
                    isSpace
                    pectg <- (return True <* string "\\%") <|> (return False)
                    isSpace
                    case rmd of Nothing -> return (Const (combineDig fst_part % 1))
                                Just v -> if pectg then return (Const (combineValue/100)) else return (Const combineValue)
                                            where combineValue = combineDig fst_part % 1 + combineDig v % (10 ^ length v))
                <|>
                (do _ <- string "."
                    rmd <-  many digit
                    isSpace
                    return (Const (combineDig rmd % (10 ^ length rmd))))
                <|>
                (do _ <- string "\\frac"
                    rmd1 <- digits
                    rmd2 <- digits
                    isSpace
                    return (BinOp Division rmd1 rmd2))

isSpace :: Parser ()
isSpace = ((string " " <|> string "\\t" <|> string "\\ ") *> isSpace) <|> return ()

combineDig = foldl (\x y -> x*10 + y) 0

oprs = [[infixL "-" Subtraction, infixL "+" Addition] -- minus and addition
        ,[infixL "\\cdot" Multiplication, infixL "*" Multiplication,infixL "/" Division] -- multiplication, division
        ,[infixL "^" Exponentiation]]
infixL str opr = InfixL (return (BinOp opr) <* string str <* isSpace)

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
genFeedback (question, solution)  mStrs rsp = reTime $
                  case Map.lookup "answer" mStrs of
                      Just v -> case runParser numeric_value_parser "" v of
                                  Left e -> markWrong $ rsp{prFeedback = [FText ("You entered " ++ show v ++ ", parse error" ++ errorBundlePretty e)]}
                                  Right userAnswer -> case evalRational userAnswer of
                                                        Just userSolution -> if userSolution == solution then  
                                                                                markCorrect $  
                                                                                    rsp{prFeedback = [FText ("Congratulations! You entered " ++ show userAnswer ++ ", the right answer is " ++ show solution)]}  
                                                                            else markWrong $ 
                                                                                    rsp{prFeedback = [FText ("Sorry! You entered " ++ show userAnswer ++ ", the answer is wrong")]}  
                                     
                      Nothing -> error "Answer field expected"
