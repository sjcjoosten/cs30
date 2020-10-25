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

data NumbericOper = Multiplication | Division | Addition | Subtraction
                   | Exponentiation deriving (Show)

digit :: Parser Integer
digit = do a <- satisfy satiDigit
           return (toInteger ((fromEnum a - fromEnum '0')))

satiDigit a =  num >= 0 && num < 10
            where num = fromEnum a - fromEnum '0'

-- 0 \cdot 667
-- 66.7%
-- \frac{1}{3}

numeric_value_parser :: Parser NumericExpression
numeric_value_parser = (do fst_part <- many digit
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
                            rmd1 <- many digit
                            rmd2 <- many digit
                            isSpace
                            return (BinOp Division rmd1 rmd2))




isSpace :: Parser ()
isSpace = ((string " " <|> string "\\t" <|> string "\\ ") *> isSpace) <|> return ()

combineDig = foldl (\x y -> x*10 + y) 0

genFeedback :: ProbEx
              -> Map.Map String String
              -> ProblemResponse
              -> ProblemResponse
genFeedback (ProbAsRational ques, answer)  mStrs rsp = reTime $
                  case Map.lookup "answer" mStrs of
                      Just v -> case parse numeric_value_parser "" v of
                                  Left e -> markWrong rsp{prFeedback = [FText ("You entered " ++ show v ++ ", parse error" ++ show e)]}
                                  Right userAnswer ->
                                      markCorrect $
                                      rsp{prFeedback = [FText ("You entered " ++ show userAnswer ++ ", the right answer is " ++ show answer)]} --todo change the time from 0 to sth
                      Nothing -> error "Answer field expected"
