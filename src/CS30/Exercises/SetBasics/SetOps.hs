module CS30.Exercises.SetBasics.SetOps where
import           CS30.Data
import           CS30.Exercises.Data
import           CS30.Exercises.Util
import           Data.List
import qualified Data.Set as Set

allDigits :: [Int]
allDigits = [1..9]

setop :: [ChoiceTree ([Field], [String])]
setop = setop_f (threeSets allDigits) ++ setop_f threeSets_oneEmpty
  where
   setop_f ts
      = [ fmap (\(digits1,digits2,digits3) -> let set1 = sort$ map show $ nub (digits1 ++ digits2)
                                                  set2 = sort$ map show $ nub (digits2 ++ digits3)
                                                  inter = Set.toList$
                                                          Set.union (Set.fromList digits2)
                                                                    (Set.intersection (Set.fromList digits1) (Set.fromList digits3))
                                               in ([FText "the set ",FMath$ dispSet set1 ++" \\cap "++dispSet set2],map show inter)
               ) ts
        , fmap (\(digits1,digits2,digits3) -> let set1 = sort$ map show $ nub (digits1 ++ digits2)
                                                  set2 = sort$ map show $ nub (digits2 ++ digits3)
                                                  uni = Set.toList$
                                                          Set.union (Set.fromList digits2)
                                                                    (Set.union (Set.fromList digits1) (Set.fromList digits3))
                                               in ([FText "the set ",FMath$ dispSet set1 ++" \\cup "++dispSet set2],map show uni)
               ) ts
        , fmap (\(digits1,digits2,digits3) -> let set1 = sort$ map show $ nub (digits1 ++ digits2)
                                                  set2 = sort$ map show $ nub (digits2 ++ digits3)
                                                  uni = Set.toList$
                                                          (Set.difference (Set.fromList digits1) (Set.fromList digits3))
                                               in ([FText "the set ",FMath$ dispSet set1 ++" \\setminus "++dispSet set2],map show uni)
               ) ts
        ]

-- | three disjoint sets of digits, of which at least one is empty
threeSets_oneEmpty :: ChoiceTree ([Int], [Int], [Int])
threeSets_oneEmpty
  = Branch [ Branch [ replace (\digits1 -> Branch
                          [ replace (\digits2 -> Branch 
                                    [ fmap (\digits3 -> (digits1, digits2, digits3)
                                          )
                                          (getOrderedSubset ((allDigits \\ digits1) \\ digits2) no_digits3)
                                    | no_digits3 <- if emptyS == (3::Int) then [0] else [(0::Int)..3] ])
                                (getOrderedSubset (allDigits \\ digits1) no_digits2)
                          | no_digits2 <- if emptyS == 2 then [0] else [(0::Int)..3] ])
                        (getOrderedSubset allDigits no_digits1)
                    | no_digits1 <- if emptyS == 1 then [0] else [(0::Int)..3] ]
          | emptyS <- [1..3]  ]
