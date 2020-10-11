module CS30.Exercises.SetBasics.Roster (roster) where
import           CS30.Data
import           CS30.Exercises.Data
import           CS30.Exercises.Util
import           Data.List

allDigits :: [Int]
allDigits = [0..9]
-- | first two levels are without duplicates, then three levels with duplicates
roster :: [ChoiceTree ([Field], [String])] -- question and solution pairs
roster = fmap (fmap (\(str,ans) -> ([FText str], ans)))
            [ Branch [ nodes [ ("the set of letters in the word "++show word, map (:[]) word)
                             | word <- withoutDoubleLetters ]
                     , Branch [ fmap (\digits -> let nr = concatMap show digits in ("the set of digits in the number "++nr, map (:[]) nr))
                                     (tailBranch  -- using tailBranch so we don't get the leading zero in numbers
                                        (getOrderedSubset allDigits no_digits))
                              | no_digits <- [(2::Int)..7] ]
                     ]
            , Branch [ Branch [ nodes [ ("the set of even numbers between "++show start++" and "++show end++" (inclusive)", [show nr | nr <- [start..end], even nr])
                                      , ("the set of odd numbers between "++show start++" and "++show end++" (inclusive)", [show nr | nr <- [start..end], odd nr])
                                      , ("the set of numbers between "++show start++" and "++show end++" (inclusive)", [show nr | nr <- [start..end]])
                                      ]
                              | end <- [start+2 .. start+10] ]
                     | start <- allDigits
                     ]
            , Branch [ nodes [ ("the set of letters in the word "++show word, map (:[]) word)
                             | word <- withDoubleLetters ]
                     , Branch [ fmap (\digits -> let nr = concatMap show digits in ("the set of digits in the number "++nr, map (:[]) nr))
                                     (replace addDouble (tailBranch -- using tailBranch so we don't get the leading zero in numbers
                                                           (genDigits no_digits)))
                              | no_digits <- [(1::Int)..8] -- final number is one longer because of the addDouble
                              ]
                     ]
            ]

withoutDoubleLetters,withDoubleLetters :: [String]
(withoutDoubleLetters,withDoubleLetters) = partition (\noun -> nub noun == noun) commonNouns

genDigits :: Int -> ChoiceTree [Int]
genDigits n | n > 0 = Branch [fmap (o:) (genDigits (n-1)) | o<-allDigits]
genDigits _ = Node []
-- | duplicate one of the elements in the list, put it in a random place
addDouble :: [a] -> ChoiceTree [a]
addDouble lst = Branch [ nodes [ take n lst++l:drop n lst
                               | n<-[0..length lst]]
                       | l<-lst ]
