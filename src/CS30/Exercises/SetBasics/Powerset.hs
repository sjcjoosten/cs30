module CS30.Exercises.SetBasics.Powerset (powset) where
import           CS30.Data
import           CS30.Exercises.Data
import           Data.List.Extra (nubSort)

allDigits :: [Int]
allDigits = [0..9]
powset :: [ChoiceTree ([Field], [[String]])]
powset = [ nodes [ ( [FText "the powerset 𝒫",FMath$ "(\\left\\{"++d1++","++d2++"\\right\\})"]
                   , [[],[d1],[d2],[d1,d2]]
                   )
                 | d1 <- map show allDigits, d2 <- map show allDigits, d1 /= d2 ]
         , nodes [ ( [FText "the powerset 𝒫",FMath$ "(\\left\\{"++d1++"\\right\\})"]
                   , [[],[d1]]
                   )
                 | d1 <- map show allDigits ++ map (:[]) ['a'..'z'] ]
         , Branch [ nodes [ ( [FText "the powerset 𝒫",FMath$ "(\\left\\{"++d1++","++d2++","++d3++"\\right\\})",FText " without listing duplicates",FNote "You're never supposed to write duplicates in roster notation, not even if the question contains duplicates"]
                            , nubSort (map nubSort ([[],[d1],[d2],[d1,d2]]++(map (d3:) [[],[d1],[d2],[d1,d2]])))
                            )
                          | d2 <- map show allDigits, d3 <- map show allDigits, d1 == d2 || d1 == d3 || d2 == d3 ]
                  | d1 <- map show allDigits ]
         ]