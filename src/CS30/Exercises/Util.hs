module CS30.Exercises.Util where
import CS30.Exercises.Data
import CS30.Data
import Data.List ( (\\), intercalate, nub, sortOn )

-- | Calculate the ProblemResponse time in a simple way
reTime :: ProblemResponse -> ProblemResponse
reTime pr = pr{prTimeToRead = length (prFeedback pr)+2} -- todo: can be much more sophisticated

-- | Display a set of things in roster notation
dispSet :: [String] -> String
dispSet sol = "\\left\\{"++intercalate ", " (nub sol)++"\\right\\}"

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
--   Warning: this ignores duplicates and creates empty branches if there arent' enough distinct items to draw from
getOrderedSubset :: Eq a 
                 => [a] -- ^ elements to draw from
                 -> Int -- ^ n: the number of elements to choose
                 -> ChoiceTree [a] -- ^ resulting tree with all elements being n large
getOrderedSubset opts n | n > 0 = Branch [fmap (o:) (getOrderedSubset lst (n-1)) | (o,lst)<-select opts]
                        | otherwise = Node []

-- | Generates a ChoiceTree in which each element is an unordered subset, with elements taken from the provided list
--   This function preserves the order in the initial list
--   For instance, 'getOrderedSubset [1,2,3,4] 2' will contain '[1,2]' but not '[1,1]' or '[2,1]'.
getUnorderedSubset :: Eq a => [a] -> Int -> ChoiceTree [a]
getUnorderedSubset opts n = map snd . sortOn fst <$> getOrderedSubset (zip [(0::Int)..] opts) n

select :: [a] -> [(a,[a])]
select [] = []
select (x:xs) = (x,xs) : [(y,x:ys) | (y,ys) <- select xs]

selectSplit :: [a] -> [([a],a,[a])]
selectSplit [] = []
selectSplit (x:xs) = ([],x,xs) : [(x:lys,y,rys) | (lys,y,rys) <- selectSplit xs]

permutations :: Int -> ChoiceTree [Int]
permutations 0 = Node []
permutations n -- ChoiceTree is a Monad now! I've also derived "Show", so you can more easily check this out in GHCI.
 = do i <- nodes [0..n-1]
      rm <- permutations (n-1)
      return (i : map (\v -> if v >= i then v+1 else v) rm)
