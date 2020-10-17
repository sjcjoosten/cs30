{-# LANGUAGE TemplateHaskell #-}
module CS30.Exercises.Probability_compute_expression where
import CS30.Data
import CS30.Exercises.Data
import qualified Data.Map as Map
import           Data.Aeson.TH -- for deriveJSON
import Debug.Trace
import Data.Char (isNumber)

data ProbEx = ProbEx deriving Show

$(deriveJSON defaultOptions ''ProbEx)

data NumericExpression = NENumber Rational | NEAdd NumericExpression NumericExpression -- stands for a + b where a and b are NumericExpression

data ParseResult a = PRError String | PRSuccess a

-- example : 2
-- example : 0.75+5\cdot 0.25+2^{24}
parser :: String -> ParseResult NumericExpression
parser [] = PRError "No number found"
parser [a] | isNumber a = PRSuccess (NENumber (read [a]))
           | otherwise = PRError ("Don't know what the number is: " ++ show a)
parser (a:as) = ..

data Parser a = P {parse :: (String -> ParseResult (a,String))}

parse (a:as) =  

getc :: Parser Char
getc = P f
       where f [] = PRError "Expecting character"
             f (c : cs) = PRSuccess (c , cs)

sat :: (Char -> Bool) -> Parser Char
sat p = check p getc

check :: (a -> Bool) -> Parser a -> Parser a
check p ps =  ps >>== f
            where f v = if p v then return v
                        else  (P(\rmd -> PRError "Fail"))

--instance Monad Parser where
(>>==) :: Parser a -> (a -> Parser b) -> Parser b
P parser1 >>== f = P f2
        where f2 str = case parser1 str of
                        PRError a -> PRError a
                        PRSuccess (v, rmd) -> f v

newReturn :: a -> Parser a
newReturn a = m (\str -> PRSuccess (a, str))

digit :: Parser Integer
digit = sat isNumber >>== (\d -> newReturn $
                            case d of
                                '0' -> 0
                                '1' -> 1)

period :: Parser Char
period = sat (== '.')

--twoDigits
        -- = digit >>= (\d1 ->
        --     digit >>= (\d2 ->
        --     return (d1, d2)))
--        = do d1 <- digit
--             d2 <- digit
--             return (d1, d2)

--1.6
--twoDigitsWithPeriod :: Parser Rational
             -- = digit >>= (\d1 ->
             --     period >>= (\_ ->
             --     digit >>= (\d2 ->
             --     return ((d1 % 1)+ (d2 % 10)))))
--twoDigitsWithPeriod = do d1 <- digit
--                      _ <- period
--                      d2 <- digit
--                      retuurn ((d1 % 1)+ (d2 % 10))


(<|>) :: Parser a -> Parser a -> Parser a
P a  <|> P b = P aOrB
        where aOrB s = case a s of
                        PRError st -> b s
                        PRSuccess v -> PRSuccess v

--- example : 11.3
asManyDigitAsILike :: Parser [Integer]
asManyDigitAsILike
     = do d1 <- digit
          more <- asManyDigitAsILike
          newReturn (d1 : more)
       <|> newReturn []

many :: Parser a -> Parser [a]
many ps
     = do d1 <- digit
          more <- asManyDigitAsILike
          newReturn (d1 : more)
       <|> newReturn []


probaEx :: ExerciseType
probaEx = exerciseType "ProbaCompute" "L?.?" "Probability : compute expression"
            choiceTreeList
            genQuestion
            genFeedback

choiceTreeList :: [Node ProbEx]


genQuestion :: ProbEx -> Exercise -> Exercise
genQuestion _ ex = ex{eQuestion = [FText "Stub question, but type your answer below anyways:" , FFieldMath "answer"]}

genFeedback :: ProbEx
                -> Map.Map String String
                -> ProblemResponse
                -> ProblemResponse
genFeedback _ mStrs rsp = trace ("genfeedback " ++ show mStrs) $
                    case Map.lookup "answer" mStrs of
                        Just v -> case parse (parser v) of
                                    PRError e -> undefined
                                    PRSuccess (exp, "") ->
                                        markCorrect $
                                        rsp{prFeedback = [FText ("You entered " ++ show v)]}
                        Nothing -> error "Answer field expected"
