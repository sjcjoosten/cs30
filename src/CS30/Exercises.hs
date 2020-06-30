{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
module CS30.Exercises where
import           CS30.Data
import           Data.Aeson
import           Data.Functor.Identity
import           Data.List
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.Encoding as Text
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           Util


correct,wrong :: ProblemResponse
wrong = ProblemResponse{prOutcome = POIncorrect, prFeedback = [], prTimeToRead = 0}
correct = ProblemResponse{prOutcome = POCorrect, prFeedback = [], prTimeToRead = 0}

data ChoiceTree a = Node a | Branch [ChoiceTree a] deriving Functor

nodes :: [a] -> ChoiceTree a
nodes = Branch . map Node
replace :: (t -> ChoiceTree a) -> ChoiceTree t -> ChoiceTree a
replace f (Node a) = f a
replace f (Branch lst) = Branch (map (replace f) lst)

-- | Super unsafe function! Requires its input to be a branch of at least two elements and removes the first.
tailBranch :: ChoiceTree a -> ChoiceTree a
tailBranch (Branch (_:lst@(_:_))) = Branch lst
tailBranch _ = error "TailBranch error"

data ExerciseType
  = ExerciseType{ etTag :: String
                , etMenu :: String
                , etTitle :: String
                , etChoices :: [ChoiceTree (Text.Text, Text.Text)]
                , etGenEx :: (Text.Text -> Exercise -> Exercise)
                , etGenAns :: (Text.Text -> Text.Text -> Map.Map String String -> ProblemResponse)
                }

-- http://math.chapman.edu/~jipsen/mathquill/test/MathQuillsymbolsMathJax.html
-- http://math.chapman.edu/~jipsen/mathquill/test/test.html

pages, tests :: [ExerciseType]
-- | Pages are publicly visible and accessible
pages = [ exerciseType "Roster" "L1.1" "Roster notation" roster (\quer ->rosterQuer [FText quer]) (\quer -> rosterFeedback [FText quer])
        , exerciseType "PowSet" "L1.2" "Powerset operations" powst rosterQuer rosterFeedback2
        , exerciseType "SetOps" "L1.3" "More set operations" setop rosterQuer rosterFeedback
        ]
 where
  -- generate three disjoint sets, none are empty
  threeSets = Branch [ replace (\digits1 -> Branch
                               [ replace (\digits2 -> Branch 
                                         [ fmap (\digits3 -> (digits1, digits2, digits3)
                                               )
                                               (genDigit ((allDigits \\ digits1) \\ digits2) no_digits3)
                                         | no_digits3 <- [(1::Int)..3] ])
                                     (genDigit (allDigits \\ digits1) no_digits2)
                               | no_digits2 <- [(1::Int)..3] ])
                             (genDigit allDigits no_digits1)
                     | no_digits1 <- [(1::Int)..3] ]
  -- generate three disjoint sets, of which at least one is empty
  threeSets_oneEmpty
   = Branch [ Branch [ replace (\digits1 -> Branch
                           [ replace (\digits2 -> Branch 
                                     [ fmap (\digits3 -> (digits1, digits2, digits3)
                                           )
                                           (genDigit ((allDigits \\ digits1) \\ digits2) no_digits3)
                                     | no_digits3 <- if emptyS == 3 then [0] else [(0::Int)..3] ])
                                 (genDigit (allDigits \\ digits1) no_digits2)
                           | no_digits2 <- if emptyS == 2 then [0] else [(0::Int)..3] ])
                         (genDigit allDigits no_digits1)
                     | no_digits1 <- if emptyS == 1 then [0] else [(0::Int)..3] ]
            | emptyS <- [1..3]  ]
  setop_f ts
   = ([ fmap (\(digits1,digits2,digits3) -> let set1 = sort$ map show $ nub (digits1 ++ digits2)
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
      ])
  setop = setop_f threeSets ++ setop_f threeSets_oneEmpty
  powst = [ nodes [ ( [FText "the powerset 𝒫",FMath$ "(\\left\\{"++d1++","++d2++"\\right\\})"]
                    , [[],[d1],[d2],[d1,d2]]
                    )
                  | d1 <- map show allDigits, d2 <- map show allDigits, d1 /= d2 ]
          , nodes [ ( [FText "the powerset 𝒫",FMath$ "(\\left\\{"++d1++"\\right\\})"]
                    , [[],[d1]]
                    )
                  | d1 <- map show allDigits ]
          , Branch [ nodes [ ( [FText "the powerset 𝒫",FMath$ "(\\left\\{"++d1++","++d2++","++d3++"\\right\\})"]
                             , nubSort (map nubSort ([[],[d1],[d2],[d1,d2]]++(map (d3:) [[],[d1],[d2],[d1,d2]])))
                             )
                           | d2 <- map show allDigits, d1 /= d2, d3 <- map show allDigits, d1 /= d3 && d2 /= d3 ]
                   | d1 <- map show allDigits]
          , Branch [ nodes [ ( [FText "the powerset 𝒫",FMath$ "(\\left\\{"++d1++","++d2++","++d3++"\\right\\})",FText " without listing duplicates",FNote "You're never supposed to write duplicates in roster notation, not even if the question contains duplicates"]
                             , nubSort (map nubSort ([[],[d1],[d2],[d1,d2]]++(map (d3:) [[],[d1],[d2],[d1,d2]])))
                             )
                           | d2 <- map show allDigits, d3 <- map show allDigits, d1 == d2 || d1 == d3 || d2 == d3 ]
                   | d1 <- map show allDigits ]
          ]
  -- first two levels are without duplicates, then three levels with duplicates
  roster = [ Branch [ nodes [ ("the set of letters in the word "++show word, map (:[]) word)
                             | word <- withoutDoubleLetters ]
                     , Branch [ fmap (\digits -> let nr = concatMap show digits in ("the set of digits in the number "++nr, map (:[]) nr))
                                     (tailBranch $ genDigit allDigits no_digits)
                              | no_digits <- [(2::Int)..7] ]
                     ]
            , Branch [ Branch [ nodes [ ("the set of even numbers between "++show start++" and "++show end++" (inclusive)", [show nr | nr <- [start..end], even nr])
                                      , ("the set of odd numbers between "++show start++" and "++show end++" (inclusive)", [show nr | nr <- [start..end], odd nr])
                                      , ("the set of numbers between "++show start++" and "++show end++" (inclusive)", [show nr | nr <- [start..end]])
                                      ]
                              | end <- [start+2 .. start+10] ]
                     | start <- allDigits
                     ]
            , nodes [ ("the set of letters in the word "++show word, map (:[]) word)
                    | word <- withDoubleLetters ]
            , Branch [ fmap (\digits -> let nr = concatMap show digits in ("the set of digits in the number "++nr, map (:[]) nr))
                            (replace addDouble $ tailBranch $ genDigits no_digits)
                     | no_digits <- [(1::Int)..8] -- final number is one longer because of the addDouble
                     ]
            ]
  genDigit opts n | n > 0 = Branch [fmap (o:) (genDigit (delete o opts) (n-1)) | o<-opts]
  genDigit _ _ = Node []
  genDigits n | n > 0 = Branch [fmap (o:) (genDigits (n-1)) | o<-allDigits]
  genDigits _ = Node []
  addDouble lst = Branch [ nodes [ take n lst++l:drop n lst
                                 | n<-[0..length lst]]
                         | l<-lst ]
-- | Tests are not publicly visible and need to be accessed through canvas
tests = []

exerciseType
      :: (ToJSON a, FromJSON a, ToJSON b, FromJSON b)
      => String -> String -> String
      -> [ChoiceTree (a, b)]
      -> (a -> Exercise -> Exercise)
      -> (a -> b -> Map.Map String String -> ProblemResponse)
      -> ExerciseType
exerciseType tg mn rn ct exGen fbGen
 = ExerciseType tg mn rn (map (fmap encPair) ct) exGen' fbGen'
 where encPair (x,y) = (Text.decodeUtf8 $ encode x, Text.decodeUtf8 $ encode y)
       exGen' :: Text.Text -> Exercise -> Exercise
       exGen' t = case decode (Text.encodeUtf8 t) of
                    Just t' -> exGen t'
                    Nothing -> error "Decoding error of the exercise (with poor debug trace)"
       fbGen' :: Text.Text -> Text.Text -> Map.Map String String -> ProblemResponse
       fbGen' a b
         = case decode (Text.encodeUtf8 a) of
             Just a' -> case decode (Text.encodeUtf8 b) of
                          Just b' -> fbGen a' b'
                          Nothing -> error "Decoding error of the solution (with poor debug trace)"
             Nothing -> error "Decoding error of the exercise (with poor debug trace)"

reTime :: ProblemResponse -> ProblemResponse
reTime pr = pr{prTimeToRead = length (prFeedback pr)+2} -- todo: can be much more sophisticated

dispSet :: [String] -> String
dispSet sol = "\\left\\{"++intercalate ", " (nub sol)++"\\right\\}"

rosterQuer :: [Field] -> Exercise -> Exercise
rosterQuer quer def = def{ eQuestion = [ FText $"Write "] ++quer++[FText " in roster notation", FFieldMath "roster" ] }
rosterFeedback :: [Field] -> [String] -> Map.Map String String -> ProblemResponse
rosterFeedback quer sol usr'
  = reTime$ case pr of
                 Nothing -> wrong{prFeedback= rsp++(FText "Your answer was "):rspwa}
                 Just v -> if nub v == v then
                              (if Set.fromList v == Set.fromList sol then correct{prFeedback=rsp}
                               else wrong{prFeedback=rsp++[FText$ ". You answered a different set: "]++rspwa})
                           else wrong{prFeedback=rsp++[FText ". Your answer contained duplicate elements: "]++rspwa}
  where solTeX = dispSet sol
        usr = Map.lookup "roster" usr'
        pr :: Maybe [String]
        pr = case usr of
               Nothing -> Nothing
               Just v -> case getSet (Text.pack v) of
                           Left _ -> Nothing -- error (errorBundlePretty str)
                           Right st -> Just (map Text.unpack st)
        rsp = [FText $ "In roster notation, "]++quer++[FText " is ", FMath solTeX]
        rspwa = case usr of
                Nothing -> [FText "- ??? - (perhaps report this as a bug?)"]
                Just v -> [FMath v]
rosterFeedback2 :: [Field] -> [[String]] -> Map.Map String String -> ProblemResponse
rosterFeedback2 quer sol usr'
  = reTime$ case pr of
                 Nothing -> wrong{prFeedback= rsp++(FText "Your answer was "):rspwa}
                 Just v -> if nub (map (nubSort) v) == (map sort v) then
                              (if Set.fromList (map Set.fromList v) == Set.fromList (map Set.fromList sol) then correct{prFeedback=reverse (drop 2 (reverse (rsp))) }
                               else wrong{prFeedback=rsp++[FText$ ". You answered a different set: "]++rspwa})
                           else wrong{prFeedback=rsp++[FText ". Your answer contained duplicate elements: "]++rspwa}
  where solTeX = dispSet (map dispSet sol)
        usr = Map.lookup "roster" usr'
        pr :: Maybe [[String]]
        pr = case usr of
               Nothing -> Nothing
               Just v -> case getSet (Text.pack v) of
                           Left _ -> Nothing
                           -- Left str -> error (errorBundlePretty str)
                           Right st -> Just (map getSet2 st)
        getSet2 x = case getSet x of
                      Left _ -> ["???"] -- a value that is not equal to a set given in the answer model
                      Right st -> (map Text.unpack st)
        rsp :: [Field]
        rsp = [FText $ "In roster notation, "]++quer++[FText " is ", FMath solTeX]
        rspwa = case usr of
                Nothing -> [FText "- ??? - (perhaps report this as a bug?)"]
                Just v -> [FMath v]

allDigits :: [Int] -- fix the type
allDigits = [0..9]
commonNouns :: [String] -- 100 most common nouns, plus a few of my own
commonNouns
 = [ "time", "year", "people", "way", "day", "man", "thing", "woman", "life", "child", "world", "school", "state", "family", "student", "group", "country", "problem", "hand", "part", "place", "case", "week", "company", "system", "program"
   , "question", "work", "science", "number", "night", "point", "home", "water", "room", "mother", "area", "money", "story", "fact", "month", "lot", "right", "study", "book", "eye", "job", "word", "business", "issue", "side"
   , "love", "head", "house", "service", "friend", "father", "power", "hour", "game", "line", "end", "member", "law", "car", "city", "community", "name", "president", "team", "minute", "idea", "kid", "body", "information"
   , "back", "parent", "face", "others", "level", "office", "door", "health", "person", "art", "computer", "history", "party", "result", "change", "morning", "reason", "research", "girl", "guy", "moment", "air", "teacher", "force", "education"
   , "solution", "answer", "library", "math" ]

withoutDoubleLetters,withDoubleLetters :: [String]
(withoutDoubleLetters,withDoubleLetters) = partition (\noun -> nub noun == noun) commonNouns

-- get set in roster notation as a list of strings, duplicates are kept, whitespace, quotes and parentheses are removed when appropriate
-- (curly) brackets work like parentheses:
-- \{1,{2,3}\} is the set containing "1" and "{2,3}"
-- \{1,\{2,3\}\} is the set containing "1" and "\{2,3\}"
-- \{1(,)2\} is a singleton set
-- All quotes (single and double) are interpreted as ticks: \{'1,2'\} contains the elements "'1" and "2'"
-- If desired, write \{{'1,2'}\} for the singleton set with "'1,2'".
getSet :: Text.Text -> Either (ParseErrorBundle Text.Text Void) [Text.Text]
getSet = fmap removeQuotes . parse parseSet ""

filterEmptystr :: [Text.Text] -> [Text.Text]
filterEmptystr [a] | a == mempty = []
filterEmptystr x = x

removeQuotes :: [Text.Text] -> [Text.Text]
removeQuotes lst
 = filterEmptystr$
   map (\v -> foldr trim v [" ","\\ ","\\left","\\right"])$
   foldr removePotential 
         (foldr removePairs 
                (map (\v -> foldr trim v [" ","\\ ","\\left","\\right"]) lst)
                [("(",")")]) -- yes, putting in {} is just latex, but we really really want to punish those writing \{{a},{b},{c}\} instead of \{a,b,c\}
         ["\"","\\\"","'"]
 where
   trim q l
     = let l1 = Text.length q
       in Text.drop (if Text.take l1 l == q then l1 else 0)
                    (Text.dropEnd (if Text.takeEnd l1 l == q then l1 else 0) l)
   removePotential q = removePairs (q,q)
   removePairs (q1,q2) lst'
     = let -- l1,l2 :: Int64
           l1 = Text.length q1
           l2 = Text.length q2
       in if and [Text.takeEnd l2 l == q1 && Text.take l1 l == q2 | l<-lst']
          then removePairs (q1,q2) [Text.drop l1 (Text.dropEnd l2 l) | l<-lst']
          else lst

-- todo: move to latex/math-parser library
type Parser = ParsecT Void Text.Text Identity

parseSet :: Parser [Text.Text]
parseSet = parseWS *> (    (string "\\{" *> parseInnerSet <* string "\\}")
                       <|> (string "{" *> parseInnerSet <* string "}") -- shouldn't be writable in math mode but we're allowing it for accessibility
                      ) <* parseWS
parseWS :: Parser Text.Text
parseWS = ((<>) <$> (string " " <|> string "\n" <|> string "\r" <|> string "\t" <|> string "\\ " <|> string "\\left" <|> string "\\right" <|> string "\\Left" <|> string "\\Right") 
                <*> parseWS -- todo: parse LaTeX comments starting with %. Problematic because escaped % should be kept. Solution for now is to keep %.
          ) <|> return mempty
parseInnerSet :: Parser [Text.Text]
parseInnerSet = sepBy (parseSatisfy) (string ",")
  where
    parseSatisfy'
      = parseSatisfy >>= trm
      where trm t = (do c <- (<>) <$> string "," <*> parseSatisfy'
                        return (t <> c)) <|> return t
    parseSatisfy
      = parseWS *> 
        (((<>) <$> (((\a b c -> a<>b<>c) <$> string "\\{" <*> parseSatisfy' <*> string "\\}")
              <|> (((\a b c -> a<>b<>c) <$> string "{" <*> parseSatisfy' <*> string "}"))
              <|> ((\a b c -> a<>b<>c) <$> string "(" <*> parseSatisfy' <*> string ")")
              <|> ((\a b c -> a<>b<>c) <$> string "[" <*> parseSatisfy' <*> string "]")
              <|> (Text.singleton <$> (notFollowedBy (exclusions) *> anySingle)))) <*> parseSatisfy
        <|> return mempty)
    exclusions = foldl1' (<|>) (map string ["{","}","(",")","[","]","\\{","\\}",","])
