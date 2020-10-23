module CS30.Exercises.Util where
import CS30.Exercises.Data
import CS30.Data
import Data.List
import Data.Ratio
import Numeric

-- | Calculate the ProblemResponse time in a simple way
reTime :: ProblemResponse -> ProblemResponse
reTime pr = pr{prTimeToRead = length (prFeedback pr)+2} -- todo: can be much more sophisticated

-- | Display a set of things in roster notation
dispSet :: [String] -> String
dispSet sol = "\\left\\{"++intercalate ", " (nub sol)++"\\right\\}"

-- | Display a rational number
dispRat :: Rational -> String -> String
dispRat sol solType
      | solType == "frac" = "\\frac{" ++ show (numerator sol) ++ "}{" ++ show (denominator sol) ++ "}"
      | solType == "money" = "$" ++ (showFFloatAlt (Just (2::Int)) (fromRational sol)) ""
      | solType == "dec" = (showFFloat (Just (5::Int)) (fromRational sol)) ""
      | otherwise = "\\frac{" ++ show (numerator sol) ++ "}{" ++ show (denominator sol) ++ "}"

-- | Produces a list with the 100 most common nouns in English, plus a few of my own.
-- | This function has no case-sensitive words, all words are in lower-case and between 3 and 11 letters in length.
commonNouns :: [String]
commonNouns
 = [ "time", "year", "people", "way", "day", "man", "thing", "woman", "life", "child", "world", "school"
   , "state", "family", "student", "group", "country", "problem", "hand", "part", "place", "case", "week"
   , "company", "system", "program", "question", "work", "science", "number", "night", "point", "home", "water"
   , "room", "mother", "area", "money", "story", "fact", "month", "lot", "right", "study", "book", "eye", "job"
   , "word", "business", "issue", "side", "love", "head", "house", "service", "friend", "father", "power"
   , "hour", "game", "line", "end", "member", "law", "car", "city", "community", "name", "president", "team"
   , "minute", "idea", "kid", "body", "information", "back", "parent", "face", "others", "level", "office"
   , "door", "health", "person", "art", "computer", "history", "party", "result", "change", "morning", "reason"
   , "research", "girl", "guy", "moment", "air", "teacher", "force", "education"
   -- words I added include:
   , "solution", "answer", "library", "math", "university", "college", "lecture", "professor", "victory"
   , "result" ]

-- | ChoiceTree to obtain three disjoint sets of choices, none are empty.
-- | The result will be shuffled
--   Not efficient in selection of input: if the input argument contains >10000 elements then this fnction needs to be rewritten.
threeSets :: Eq a => [a] -> ChoiceTree ([a], [a], [a])
threeSets allDigits = Branch [ replace (\digits1 -> Branch
                                       [ replace (\digits2 -> Branch 
                                                 [ fmap (\digits3 -> (digits1, digits2, digits3)
                                                       )
                                                       (getOrderedSubset ((allDigits \\ digits1) \\ digits2) no_digits3)
                                                 | no_digits3 <- [(1::Int)..max_per_set] ])
                                             (getOrderedSubset (allDigits \\ digits1) no_digits2)
                                       | no_digits2 <- [(1::Int)..max_per_set] ])
                                     (getOrderedSubset allDigits no_digits1)
                             | no_digits1 <- [(1::Int)..max_per_set] ]
                             where max_per_set = length allDigits `div` 3

-- | Generates a ChoiceTree in which each element is an ordered subset, with elements taken from the provided list
--   For instance, 'getOrderedSubset [1,2,3,4] 2' will contain '[2,1]' but not '[1,1]'.
getOrderedSubset :: Eq a 
                 => [a] -- ^ elements to draw from
                 -> Int -- ^ n: the number of elements to choose
                 -> ChoiceTree [a] -- ^ resulting tree with all elements being n large
getOrderedSubset opts n | n > 0 = Branch [fmap (o:) (getOrderedSubset (delete o opts) (n-1)) | o<-opts]
getOrderedSubset _ _ = Node []
