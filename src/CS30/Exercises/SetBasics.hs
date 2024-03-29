module CS30.Exercises.SetBasics (rosterEx,powsetEx,setOpsEx) where
import CS30.Data (Field(..),Exercise(..))
import CS30.Exercises.Data (ExerciseType,exerciseType)
import CS30.Exercises.SetBasics.Powerset
import CS30.Exercises.SetBasics.Roster
import CS30.Exercises.SetBasics.SetOps
import CS30.Exercises.SetBasics.SolutionChecker

rosterEx,powsetEx,setOpsEx :: ExerciseType
rosterEx = exerciseType "Roster" "Optional" "Roster notation" roster rosterQuer rosterFeedback
powsetEx = exerciseType "Powset" "L1.1" "Powerset operations" powset rosterQuer (rosterFeedback2 False)
setOpsEx = exerciseType "SetOps" "L1.2" "More set operations" setops rosterQuer rosterFeedback

rosterQuer :: ([Field],a) -> Exercise -> Exercise
rosterQuer (quer, _solution) def 
 = def{ eQuestion = [ FText $"Write "] ++quer++
                    [ FText " in roster notation", FFieldMath "roster" ] }
