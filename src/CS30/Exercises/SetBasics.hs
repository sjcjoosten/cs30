module CS30.Exercises.SetBasics (rosterEx,powsetEx,setOpsEx) where
import CS30.Data (Field(..),Exercise(..))
import CS30.Exercises.Data (ExerciseType,exerciseType)
import CS30.Exercises.SetBasics.Powerset
import CS30.Exercises.SetBasics.Roster
import CS30.Exercises.SetBasics.SetOps
import CS30.Exercises.SetBasics.SolutionChecker

rosterEx,powsetEx,setOpsEx :: ExerciseType
rosterEx = exerciseType "Roster" "L1.1" "Roster notation" roster rosterQuer    rosterFeedback
powsetEx = exerciseType "PowSet" "L1.2" "Powerset operations" powerset rosterQuer rosterFeedback2
setOpsEx = exerciseType "SetOps" "L1.3" "More set operations" setop rosterQuer rosterFeedback

rosterQuer :: ([Field],a) -> Exercise -> Exercise
rosterQuer (quer, _solution) def 
 = def{ eQuestion = [ FText $"Write "] ++quer++
                    [ FText " in roster notation", FFieldMath "roster" ] }
