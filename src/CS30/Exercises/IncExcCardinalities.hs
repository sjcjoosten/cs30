{-# LANGUAGE TemplateHaskell #-}
module CS30.Exercises.IncExcCardinalities (incExcCards) where
import CS30.Data
import CS30.Exercises.Data
import Data.Aeson as JSON -- don't think I use this
import Data.Aeson.TH

data IncExcProblem = IEP ([[Field]], String)
$(deriveJSON defaultOptions ''IncExcProblem)

possibleValues :: [Int]
possibleValues = [1..200]

choiceTreeList :: [ChoiceTree IncExcProblem]
choiceTreeList = [ 
      -- Question type 1: Given |A|, |B|, and |A U B|, find |A ∩ B|.
      nodes [IEP ([
                        [FText"|A| ", FMath$ "= "++show(d1)], 
                        [FText"|B| ", FMath$"= "++show(d2)], 
                        [FMath$ "|A \\cup B|", FMath$"= "++show(d3)],
                        [FMath$ "|A \\cap B|"]
                  ], show ((d1 + d2) - d3)) 
            | d1 <- possibleValues, d2 <- possibleValues, d3 <- possibleValues, d3 < d1 + d2]
      ]

incExcCards :: ExerciseType
incExcCards
  = exerciseType "Inclusion exclusion cardinalities" "L?.?"
              "Inclusion exclusion principle" 
              choiceTreeList
              genExercise 
              unknownFeedback
        where unknownFeedback _ _ pr = pr

genExercise :: IncExcProblem -> Exercise -> Exercise
genExercise (IEP (fields, answer)) def 
 = def{ eQuestion = [ FText $"Given the following cardinalities"
                    , FTable (setCards)
                    , FText $"Give the cardinality of "
                    , answerField
                    , FFieldMath "answer" ]
      , eBroughtBy = ["Rachael, Tyler"] }
 where setCards = (map . map) Cell (init fields) -- Displays the fields in a table.
       answerField = (head . last) fields -- Last field of IEP is not displayed, but rather is the expression that the user is told to find the value of.