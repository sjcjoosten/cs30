{-# LANGUAGE OverloadedStrings #-}
module CS30.Exercises.SetBasics.SolutionChecker (rosterFeedback, rosterFeedback2) where
import           CS30.Data
import           CS30.Exercises.Util
import           Data.Functor.Identity
import           Data.List
import           Data.List.Extra (nubSort)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.Text.Lazy as Text
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char -- readFile
-- TODO: get rid of code duplication

-- | Answer whether a user-given set matches the set in the solution for sets of elements (no sets within sets)
rosterFeedback :: ([Field], [String]) -> Map.Map String String -> ProblemResponse -> ProblemResponse
rosterFeedback (quer, sol) usr' defaultRsp
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
        wrong = markWrong defaultRsp
        correct = markCorrect defaultRsp
-- | Answer whether a user-given set matches the set in the solution for sets of sets (no sets within those, just elements)
rosterFeedback2 :: ([Field], [[String]]) -> Map.Map String String -> ProblemResponse -> ProblemResponse
rosterFeedback2 (quer, sol) usr' defaultRsp
  = reTime$ case pr of
                 Nothing -> wrong{prFeedback= rsp++(FText "Your answer was "):rspwa}
                 Just v -> if nub (map (nubSort) v) == (map sort v) then
                              ( if Set.fromList (map Set.fromList v) == Set.fromList (map Set.fromList sol)
                                then correct{prFeedback=rsp }
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
        wrong = markWrong defaultRsp
        correct = markCorrect defaultRsp


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

type Parser = ParsecT Void Text.Text Identity

parseSet :: Parser [Text.Text]
parseSet = parseWS *> (    (string "\\{" *> parseInnerSet <* string "\\}")
                       <|> (string "{" *> parseInnerSet <* string "}") -- shouldn't be writable in math mode but we're allowing it for accessibility
                       <|> (string "\\emptyset" *> return [])
                       <|> (string "\\Emptyset" *> return [])
                      ) <* parseWS
parseWS :: Parser Text.Text
parseWS = ((<>) <$> (string " " <|> string "\n" <|> string "\r" <|> string "\t" <|> string "\\ " <|> string "\\left" <|> string "\\right" <|> string "\\Left" <|> string "\\Right"
                     <|> ((<>) <$> string "%" <*> untilEOL)) 
                <*> parseWS
          )
          <|> return mempty
        where untilEOL = (do c <- Text.singleton <$> anySingle
                             if c == "\n" || c == "\r" then return c else (c <>) <$> untilEOL)
                         <|> return mempty
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
