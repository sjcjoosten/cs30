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


correct,wrong :: ProblemResponse
wrong = ProblemResponse{prOutcome = POIncorrect,prFeedback = [], prTimeToRead = 0}
correct = ProblemResponse{prOutcome = POCorrect,prFeedback = [], prTimeToRead = 0}

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

pages, tests :: [ExerciseType]
-- | Pages are publicly visible and accessible
pages = [ rosterEx ]
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
reTime pr = pr{prTimeToRead = length (prFeedback pr)*2+2} -- todo: can be much more sophisticated

rosterFeedback :: [Char] -> [String] -> Map.Map String String -> ProblemResponse
rosterFeedback quer sol usr'
  = reTime$ case pr of
                 Nothing -> wrong{prFeedback= rsp}
                 Just v -> if nub v == v then
                              (if Set.fromList v == Set.fromList sol then correct{prFeedback=take 2 rsp}
                               else wrong{prFeedback=rsp++[FText ", which is a different set"]})
                           else wrong{prFeedback=rsp++[FText ", which contains duplicate elements"]}
  where solTeX = "\\left\\{"++intercalate ", " (nub sol)++"\\right\\}"
        usr = Map.lookup "roster" usr'
        pr :: Maybe [String]
        pr = fmap (map Text.unpack) . getSet =<< Text.pack <$> usr
        rsp = [FText $ "In roster notation, "++quer++" is ", FMath solTeX] ++
              case usr of
                Nothing -> [FText "Your answer triggered an application error (perhaps report this as a bug?)"]
                Just v -> [FText "Your answer was ", FMath v]

rosterEx :: ExerciseType
rosterEx
 = exerciseType "Roster" "L1.1" "Roster notation" puzzles
      (\quer def-> def{ eQuestion = [ FText $"Write "++quer++" in roster notation"
                                    , FFieldMath "roster" ] })
      rosterFeedback
 where
  -- first two levels are without duplicates, then three levels with duplicates
  puzzles = [ Branch [ nodes [ ("the set of letters in the word "++show word, map (:[]) word)
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

allDigits :: [Int] -- fix the type
allDigits = [0..9]
commonNouns :: [String] -- 100 most common nouns, plus a few of my own
commonNouns
 = [ "time", "year", "people", "way", "day", "man", "thing", "woman", "life", "child", "world", "school", "state", "family", "student", "group", "country", "problem", "hand", "part", "place", "case", "week", "company", "system", "program"
   , "question", "work", "science", "number", "night", "point", "home", "water", "room", "mother", "area", "money", "story", "fact", "month", "lot", "right", "study", "book", "eye", "job", "word", "business", "issue", "side"
   , "love", "head", "house", "service", "friend", "father", "power", "hour", "game", "line", "end", "member", "law", "car", "city", "community", "name", "president", "team", "minute", "idea", "kid", "body", "information"
   , "back", "parent", "face", "others", "level", "office", "door", "health", "person", "art", "computer", "history", "party", "result", "change", "morning", "reason", "research", "girl", "guy", "moment", "air", "teacher", "force", "education"
   , "solution", "answer", "library", "math" ]

withDoubleLetters,withoutDoubleLetters :: [String]
(withDoubleLetters,withoutDoubleLetters) = partition (\noun -> nub noun == noun) commonNouns

-- get set in roster notation as a list of strings, duplicates are kept, whitespace, quotes and parentheses are removed when appropriate
-- (curly) brackets work like parentheses:
-- \{1,{2,3}\} is the set containing "1" and "{2,3}"
-- \{1,\{2,3\}\} is the set containing "1" and "\{2,3\}"
-- \{1(,)2\} is a singleton set
-- All quotes (single and double) are interpreted as ticks: \{'1,2'\} contains the elements "'1" and "2'"
-- If desired, write \{{'1,2'}\} for the singleton set with "'1,2'".
getSet :: Text.Text -> Maybe [Text.Text]
getSet = fmap removeQuotes . parseMaybe parseSet
   
removeQuotes :: [Text.Text] -> [Text.Text]
removeQuotes lst
 = foldr removePotential 
         (foldr removePairs 
                (foldr trim lst [" ","\\ "])
                [("(",")")]) -- yes, putting in {} is just latex, but we really really want to punish those writing \{{a},{b},{c}\} instead of \{a,b,c\}
         ["\"","\\\"","'"]
 where
   trim q = removePairs (q,mempty) . removePairs (mempty,q)
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
    parseSatisfy
      = ((<>) <$> (((\a b c -> a<>b<>c) <$> string "{" <*> parseSatisfy <*> string "}")
                <|> ((\a b c -> a<>b<>c) <$> string "(" <*> parseSatisfy <*> string ")")
                <|> ((\a b c -> a<>b<>c) <$> string "[" <*> parseSatisfy <*> string "]")
                <|> (Text.singleton <$> satisfy (\c -> not (c == ',')))) <*> parseSatisfy)
        <|> return mempty