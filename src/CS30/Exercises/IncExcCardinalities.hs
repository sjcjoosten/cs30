{-# LANGUAGE TemplateHaskell #-}
module CS30.Exercises.IncExcCardinalities (incExcCardinalitiesEx) where
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
choiceTreeList = [ do inters <- nodes [10..40]
                      a <- nodes [inters+10..inters+110]
                      b <- nodes [inters+10..inters+110]
                      (given,ansExpr,ansNum)
                        <- nodes [ ([FMath$ "|A \\cup B|", FMath$"= "++show(a+b-inters)]
                                   ,(FMath$ "|A \\cap B|")
                                   ,inters)
                                 , ([FMath$ "|A \\cap B|", FMath$"= "++show(inters)]
                                   ,(FMath$ "|A \\cup B|")
                                   ,a+b-inters)]
                      return $
                        IEP [[FText"|A| ", FMath$ "= "++show(a)], 
                             [FText"|B| ", FMath$"= "++show(b)], 
                             given]
                            ansExpr
                            ansNum
                 , do aUbc <- nodes [60..90]
                      aUb <- nodes [aUbc+10..aUbc+110]
                      aUc <- nodes [aUbc+10..aUbc+110]
                      let aUbUc = aUb + aUc - aUbc
                      let computerade = [("|A \\cup C|",aUc)
                                        ,("|A \\cup B \\cup C|",aUbUc)
                                        ,("|A \\cup (B \\cap C)|",aUbc)
                                        ,("|A \\cup B|",aUb)]
                      remv <- nodes [0..3]
                      return $
                        IEP [ [FMath f, FMath$ "= "++show s]
                            | (f,s) <- take remv computerade ++ drop (remv+1) computerade]
                            (FMath (fst$ computerade!!remv))
                            (snd$ computerade!!remv)
                 ]

incExcCardinalitiesEx :: ExerciseType
incExcCardinalitiesEx
  = exerciseType "IncExcCardinalities" "L2.2"
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
