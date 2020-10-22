{-# LANGUAGE TemplateHaskell #-}
module CS30.Exercises.IncExcCardinalities (incExcCards, spaces) where
import CS30.Data
import CS30.Exercises.Data
import Data.Aeson as JSON -- don't think I use this
import Data.Aeson.TH
import Data.Void(Void)
import Data.Char
import qualified Data.Map as Map
import Debug.Trace
import Text.Megaparsec
import Text.Megaparsec.Char
import CS30.Exercises.Util

data IncExcProblem = IEP ([[Field]], Int)
$(deriveJSON defaultOptions ''IncExcProblem)

type Parser = Parsec Void String

possibleValues :: [Int]
possibleValues = [1..200]

choiceTreeList :: [ChoiceTree IncExcProblem]
choiceTreeList = [ 
      -- Question type 1: Given |A|, |B|, and |A U B|, find |A ∩ B| (or vice versa).
      Branch [ Branch [
                  nodes [IEP ([
                              [FText"|A| ", FMath$ "= "++show(d1)], 
                              [FText"|B| ", FMath$"= "++show(d2)], 
                              [FMath$ "|A \\cup B|", FMath$"= "++show(d3)],
                              [FMath$ "|A \\cap B|"]
                        ], (d1+d2-d3))], 
                  nodes [IEP ([
                              [FText"|A| ", FMath$ "= "++show(d1)], 
                              [FText"|B| ", FMath$"= "++show(d2)], 
                              [FMath$ "|A \\cap B|", FMath$"= "++show(d3)],
                              [FMath$ "|A \\cup B|"]
                        ], (d1+d2-d3))]
                  ]
             | d1 <- possibleValues, d2 <- possibleValues, d3 <- possibleValues, d3 < d1 + d2]
      ]

incExcCards :: ExerciseType
incExcCards
  = exerciseType "Inclusion exclusion cardinalities" "L?.?"
              "Inclusion exclusion principle" 
              choiceTreeList
              genExercise 
              genFeedback

genExercise :: IncExcProblem -> Exercise -> Exercise
genExercise (IEP (fields, _)) def 
 = def{ eQuestion = [ FText $"Given the following cardinalities"
                    , FTable (setCards)
                    , FText $"Give the cardinality of "
                    , answerField
                    , FFieldMath "answer" ]
      , eBroughtBy = ["Rachael, Tyler"] }
 where setCards = (map . map) Cell (init fields) -- Displays the fields in a table.
       answerField = (head . last) fields -- Last field of IEP is not displayed, but rather is the expression that the user is told to find the value of.

genFeedback :: IncExcProblem -> Map.Map String String -> ProblemResponse -> ProblemResponse
genFeedback (IEP (fields, sol)) mStrs pr 
      = reTime$ case answer of
                  Nothing -> trace ("wrong" ++ show rsp) wrong{prFeedback= rsp++(FText "Your answer was "):rspwa}
                  Just v -> if (v == sol) 
                              then trace ("correct" ++ show rsp) correct{prFeedback=rsp}
                              else trace ("wrong" ++ show rsp) wrong{prFeedback=rsp++[FText "Your answer was "]++[FMath (show v)]}
      where ans = Map.lookup "answer" mStrs
            answer :: Maybe Int
            answer = case ans of
                  Nothing -> Nothing
                  Just v -> case parse parseAnswer "" v of 
                        Left _ -> Nothing
                        Right st -> Just st
            wrong = markWrong pr
            correct = markCorrect pr
            rsp :: [Field]
            rsp = [FText $ "The cardinality of "]++[answerField]++[FText " is ", FMath (show sol), FText "."]
            rspwa = case ans of
                Nothing -> [FText "??? (perhaps report this as a bug?)"]
                Just v -> [FMath v]
            answerField = (head . last) fields

parseAnswer :: Parser Int
parseAnswer = unspace digits

unspace :: Parser a -> Parser a
unspace p = spaces >> p

spaces :: Parser ()
spaces = many spc >> return ()
 where spc = string " " <|> string "\t" <|> string "\n" <|> string "\r"
             <|> string "\\ " <|> string "~" -- when parsing latex, these display as spaces too

digits :: Parser Int
digits = do ds <- some digit
            return (foldl1 shiftl ds)
         where shiftl m n = 10*m+n

digit :: Parser Int
digit = cvt <$> satisfy isDigit
  where cvt d = fromEnum d - fromEnum '0'