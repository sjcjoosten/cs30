{-# LANGUAGE OverloadedStrings #-}
module CS30.Exercises.ProbWord.SolutionChecker (probFeedback) where
import           CS30.Data
import           CS30.Exercises.Util
import qualified Data.Map as Map


probFeedback :: ([Field], String) -> Map.Map String String -> ProblemResponse -> ProblemResponse
probFeedback (quer, sol) usr' defaultRsp
  = reTime$ case usr of 
              Nothing -> wrong{prFeedback=rsp++[FText$ ". You answered incorrectly: "]++rspwa}
              Just v -> if v == sol then correct{prFeedback=rsp} else wrong{prFeedback=rsp++[FText$ ". You answered incorrectly: "]++rspwa}
  where 
    usr = Map.lookup "prob" usr'
    rsp = [FText $ "The answer for "]++quer++[FText " is ", FText $ sol]
    rspwa = case usr of
            Nothing -> [FText "- ??? - (perhaps report this as a bug?)"]
            Just v -> [FMath v]
    wrong = markWrong defaultRsp
    correct = markCorrect defaultRsp