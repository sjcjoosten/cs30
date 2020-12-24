module CS30.Exercises.Relations.FunctionBasics (multiplicitiesFunctions) where
import CS30.Data
import CS30.Exercises.Data
import CS30.Exercises.Util
import qualified Data.Map as Map
import Control.Arrow ((***))
import Data.List ( (\\), intercalate, nub, partition )
import Data.List.Extra (nubSort)
import GHC.Exts

data ISB = Injective | Surjective | Bijective deriving Show
isb_all = [Injective,Surjective,Bijective]

multiplicitiesFunctions :: ExerciseType
multiplicitiesFunctions
 = exerciseType "multiplicitiesFunctions" "L2.2" "Multiplicities of functions"
     [do f_is <- (,) <$> boolTree <*> nodes isb_all
         g_is <- (,) <$> boolTree <*> nodes isb_all
         asked <- nodes isb_all
         let givens = [FMath "f",FText$ show_is f_is
                      ,FText " and ",FMath "g", FText$ show_is g_is]
         let question = "g\\circ{}f"
         let options = [ [FMath "f",FText$ " is " ++ show asked]
                       , [FMath "f",FText$ " is not " ++ show asked]
                       , [FText$ "We cannot tell if ",FMath "f",FText$ " is " ++ show asked]
                       ]
         return ([ FText "Let ",FMath "f : A\to B",FText " and ",FMath "g : B\to C",FText " be functions such that "
                 ] ++givens++
                 [ FText ". What can you say about ",FMath question
                 , FText "?"
                 , FChoice "Answer" options
                 ],())
     ]
     undefined undefined
  where show_is (True,v) = " is "++show v
        show_is (False,v) = " is not "++show v
