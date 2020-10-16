-- | This module doesn't really have an exercise, it is just there to show how to draw tables.
--   The exported exercise 'stub' does only that: draw a table with three rows and two columns.

{-# LANGUAGE TemplateHaskell #-}
module CS30.Exercises.IncExcCardinalities (incExcCards) where
import CS30.Data
import CS30.Exercises.Data
import Data.Aeson as JSON -- don't think I use this
import Data.Aeson.TH

data IncExcProblem = IEP
$(deriveJSON defaultOptions ''IncExcProblem)

incExcCards :: ExerciseType
incExcCards
  = exerciseType "incExcCards" "(testing)"
              "Set inclusion exclusion principle" 
              [Node IEP]
              genExercise unknownFeedback
        where unknownFeedback _ _ pr = pr
genExercise :: IncExcProblem -> Exercise -> Exercise
genExercise p def 
 = def{ eQuestion = [ FText $"Given the following cardinalities"
                    , FTable (setCards)
                    , FText $"Give the cardinality of "
                    , FMath "C"
                    , FFieldMath "answer" ]
      , eBroughtBy = ["Rachael"] }
 where _unused = p
       setCards = [[Cell (FMath "|A| = 3"),Cell (FMath "|B| = 2") ]]