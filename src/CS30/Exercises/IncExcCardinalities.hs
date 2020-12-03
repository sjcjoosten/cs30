{-# LANGUAGE TemplateHaskell #-}
module CS30.Exercises.IncExcCardinalities (incExcCards) where
import           CS30.Data
import           CS30.Exercises.Data
import           CS30.Exercises.Util
import           Data.Aeson as JSON -- used for 'deriveJSON' line
import           Data.Aeson.TH
import           Data.Char
import qualified Data.Map as Map
import           Data.Void(Void)
import           Text.Megaparsec
import           Text.Megaparsec.Char

data IncExcProblem = IEP {tbl::[[Field]], exprQ::Field, answer::Int}
$(deriveJSON defaultOptions ''IncExcProblem)

type Parser = Parsec Void String

choiceTreeList :: [ChoiceTree IncExcProblem]
choiceTreeList = [
      -- ChoiceTree of Branches for each variable in the equation |A U B U C| = |A| + |B| + |C| - |A ∩ B| - |A ∩ C| - |B ∩ C| + |A ∩ B ∩ C|, representing three sets.
      -- |A U B U C| = a + b + c - d - e - f + g
      -- It then becomes trivial to create and solve systems of equations for different question types.
      Branch [ Branch [Branch [Branch [Branch [Branch [Branch [
            nodes [
                 -- Basic part of assignment --
                 -- Question type 1: Given |A|, |B|, and |A U B|, find |A ∩ B| (or vice versa).
                   IEP [
                         [FText"|A| ", FMath$ "= "++show(a)], 
                         [FText"|B| ", FMath$"= "++show(b)], 
                         [FMath$ "|A \\cup B|", FMath$"= "++show(d)]]
                       (FMath$ "|A \\cap B|")
                       (a+b-d)
                  ],
                  
            -- Creative part of assignment: extra problem types --
            -- Question type 2: Given |A U C|, |A U B U C|, and |A U (C ∩ B)|, find |A U B|.
            nodes [IEP [[FMath "|A \\cup C|", FMath$ "= "++show(a+c-e)], 
                        [FMath "|A \\cup B \\cup C|", FMath$"= "++show(a+b+c-d-e-f+g)], 
                        [FMath "|A \\cup (C \\cap B)|", FMath$"= "++show(a+f-g)]]
                       (FMath "|A \\cup B|")
                       (a + b - d)],
            -- Question type 3: Given |A U C|, |B U C|, and |C U (A ∩ B)|, find |A U B U C|.
            nodes [IEP [[FMath "|A \\cup C|", FMath$ "= "++show(a+c-e)], 
                        [FMath "|B \\cup C|", FMath$"= "++show(b+c-f)], 
                        [FMath "|C \\cup (A \\cap B)|", FMath$"= "++show(c+d-g)]
                        ]
                       (FMath "|A \\cup B \\cup C|")
                       (a + b + c - d - e - f + g)],
            -- Question type 4: Given |A U C|, |B U C|, and |B U (A ∩ C)|, find |A U (B ∩ C)|.
            nodes [IEP [[FMath "|A \\cup C|", FMath$ "= "++show(a+c-e)], 
                        [FMath "|B \\cup C|", FMath$ "= "++show(b+c-f)],
                        [FMath "|B \\cup (A \\cap C)|", FMath$ "= "++show(b+e-g)]]
                       (FMath "|A \\cup (B \\cap C)|")
                       (a + f - g)]
      ] | f <- [g+1..min (c-50) (b-50)]]
      | e <- [g+1..min (c-50) (a-50)]]
      | d <- [g+1..min (b-50) (a-50)]]
      | c <- [120..180], c > g]
      | b <- [120..180], b > g]
      | a <- [120..180], a > g]
      | g <- [10..60]]
      -- These ranges were selected to ensure that none of the branches are empty given the constraints provided. 

incExcCards :: ExerciseType
incExcCards
  = exerciseType "IncExcCardinalities" "Rachael and Tyler"
                 "Inclusion exclusion principle" 
                 [Branch choiceTreeList]
                 genExercise 
                 genFeedback

genExercise :: IncExcProblem -> Exercise -> Exercise
genExercise iep def 
 = def{ eQuestion = [ FText $"Given the following cardinalities:"
                    , FTable (map (map Cell) (tbl iep))
                    , FText $"Give the cardinality of "
                    , exprQ iep
                    , FText $"."
                    , FFieldMath "answer" ]
      , eBroughtBy = ["Rachael", "Tyler"] }

genFeedback :: IncExcProblem -> Map.Map String String -> ProblemResponse -> ProblemResponse
genFeedback iep mStrs pr 
      = reTime$ case answer' of
                  Nothing -> wrong{prFeedback= rsp++(FText " Your answer was "):rspwa} -- Error when parsing. 
                  Just v -> if (v == answer iep) 
                              then correct{prFeedback=rsp} -- Correct answer.
                              else wrong{prFeedback=rsp++[FText " Your answer was "]++[FMath (show v), FText "."]} -- Incorrect answer.
      where ans = Map.lookup "answer" mStrs -- Raw user input. 
            answer' :: Maybe Int
            answer' = case ans of
                  Nothing -> Nothing
                  Just v -> case parse parseAnswer "" v of -- Parses the user input to look for an integer. 
                        Left _ -> Nothing
                        Right st -> Just st
            wrong = markWrong pr
            correct = markCorrect pr
            rsp :: [Field]
            rsp = [FText $ "The cardinality of ", exprQ iep, FText " is ", FMath (show (answer iep)), FText "."] -- Displays the correct answer.
            rspwa = case ans of -- Displays the raw user input. 
                Nothing -> [FText "??? (perhaps report this as a bug?)"]
                Just v -> [FMath v, FText "."]

parseAnswer :: Parser Int
parseAnswer = unspace digits
  where 
      digits = do ds <- some digit
                  return (foldl1 shiftl ds)
      shiftl m n = 10*m+n
      digit = cvt <$> satisfy isDigit
      cvt d = fromEnum d - fromEnum '0'
unspace :: Parser a -> Parser a
unspace p = spaces *> p <* spaces

spaces :: Parser ()
spaces = many spc >> return ()
 where spc = string " " <|> string "\t" <|> string "\n" <|> string "\r"
             <|> string "\\ " <|> string "~" -- when parsing latex, these display as spaces too
