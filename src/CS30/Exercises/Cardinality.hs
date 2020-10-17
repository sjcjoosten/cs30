{-# LANGUAGE TemplateHaskell #-}
module CS30.Exercises.Cardinality (cardEx) where
import           CS30.Data
import           CS30.Exercises.Data
import           Data.List.Extra (nubSort)
import qualified Data.Map as Map
import           Data.Aeson.TH
import qualified Data.Text.Lazy as Text
import           Data.Functor.Identity
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char -- readFile
import Debug.Trace


data CardExp = CardExp deriving Show
-- type CardExp a = ([Field],a)
-- $(deriveJSON defaultOptions ''CardExp)

-- not yet used, but we might want something like this
-- to parse the user's expressions
data MathExpr = Const Int 
              | Mult  MathExpr MathExpr  -- multiply two MathExpr's 
              | Expon MathExpr MathExpr  -- set first MathExpr to the second MathExpr power
              | Choose MathExpr MathExpr -- first MathExpr `choose` second MathExpr

cardEx :: ExerciseType

cardEx = exerciseType "Cardinality" "L??" "Cardinality of Expression" 
            cardinality
            cardQuer 
            cardFeedback

allCards :: [Int]
allCards = [1..99]

cardinality :: [ChoiceTree ([[Field]], [String])]
cardinality = [ 
            -- cardinality of the cartesian product of two sets
           nodes [ ( [[FText"|A| ", FMath$ "= "++d1, FText" and |B| ", FMath$"= "++d2], 
                        [FText"|", FMath$ "A x B", FText"|"]]
                     , [show (read (d1) * read(d2))] -- this actually does out the mulitplication (but idk if we necessarily want them to, smthg to think about)
                     )
                   | d1 <- map show allCards, d2 <- map show allCards]
            -- cardinality of a power set
            , nodes [ ( [[FText"|A| ", FMath$ "= "++d1], [FText "|𝒫",FMath$ "(A)", FText"|"]]
                     , ["2^"++d1] -- needs {}
                     )
                   | d1 <- map show allCards]
            -- cardinality of set x its powerset
           , Branch [ nodes [ ( [[FText"|A| ", FMath$"= "++d1],
                                [FText"|", FMath$"A x ", FText"𝒫", FMath"(A)", FText"|"]]
                              ,[d1++"*2^"++d1, -- needs \\cdot and {}
                                "2^"++d1++"*"++d1] -- needs \\cdot and {}
                              )
                            | d1 <- map show allCards]
                     ] 
            -- cardinality with set builder notatino (like ex from the assignment sheet)
           , Branch [ nodes [ ( [[FText"|B| ", FMath$ "= "++d2],
                                 [FText"|", FMath$"\\left\\{A | A \\subseteq B, |A| ="++d1++"\\right\\}", FText"|"]]
                              , [d2++" choose "++ d1]
                              )
                            | d1 <- map show allCards, d2 <- map show allCards]
                    ]
           ]


cardQuer :: ([[Field]],[String]) -> Exercise -> Exercise
cardQuer (quer, _solution) exer 
  = trace("solution " ++  show _solution) -- for testing 
    exer { eQuestion=[FText "Given "] ++ rule ++ 
                     [FText ", compute "] ++ question ++ 
                     [FFieldMath "answer"]}
                        where rule = head quer
                              question = quer!!1

-- rosterQuer :: ([Field],a) -> Exercise -> Exercise
-- rosterQuer (quer, _solution) def 
--  = def{ eQuestion = [ FText $"Write "] ++quer++
--                     [ FText " in roster notation", FFieldMath "roster" ] }


cardFeedback :: ([[Field]],[String]) -> Map.Map String String -> ProblemResponse -> ProblemResponse
cardFeedback (quer, sol) mStrs defaultRsp 
  =  trace ("gen feedback " ++ show mStrs ++ " " ++ show pr) $ -- for testing
      case pr of 
       Just v -> if v `elem` sol then 
                    markCorrect $ defaultRsp{prFeedback = [FText("you entered " ++ show v)]}
                 else markWrong $ defaultRsp{prFeedback = [FText("the correct answer is "++head sol)]}
       Nothing -> markWrong $ defaultRsp{prFeedback = [FText("the correct answer is "++head sol)]}
      where usr = Map.lookup "answer" mStrs
            pr :: Maybe String
            pr = case usr of
                   Nothing -> Nothing
                   Just v -> case parse parseMultiply "" v of
                               Left _ -> Nothing -- error (errorBundlePretty str)
                               Right st -> Just (show  ((fst st)*(snd st))) 
                               -- ^ if we parse a multiplication expression, then multiply the two numbers to see if it's right  


type Parser = ParsecT Void String Identity

-- parses a multiplication expression in the form of "12\\cdot12"
-- into a tuple containing the two multiplied numbers
parseMultiply :: Parser (Int,Int)
parseMultiply = do 
  n1 <- some digitChar
  _  <- string "\\cdot"
  n2 <- some digitChar
  return (read n1, read n2)

-- rosterFeedback :: ([Field], [String]) -> Map.Map String String -> ProblemResponse -> ProblemResponse
-- rosterFeedback (quer, sol) usr' defaultRsp
--   = reTime$ case pr of
--                  Nothing -> wrong{prFeedback= rsp++(FText "Your answer was "):rspwa}
--                  Just v -> if nub v == v then
--                               (if Set.fromList v == Set.fromList sol then correct{prFeedback=rsp}
--                                else wrong{prFeedback=rsp++[FText$ ". You answered a different set: "]++rspwa})
--                            else wrong{prFeedback=rsp++[FText ". Your answer contained duplicate elements: "]++rspwa}
--   where solTeX = dispSet sol
--         usr = Map.lookup "roster" usr'
--         pr :: Maybe [String]
--         pr = case usr of
--                Nothing -> Nothing
--                Just v -> case getSet (Text.pack v) of
--                            Left _ -> Nothing -- error (errorBundlePretty str)
--                            Right st -> Just (map Text.unpack st)
--         rsp = [FText $ "In roster notation, "]++quer++[FText " is ", FMath solTeX]
--         rspwa = case usr of
--                 Nothing -> [FText "- ??? - (perhaps report this as a bug?)"]
--                 Just v -> [FMath v]
--         wrong = markWrong defaultRsp
--         correct = markCorrect defaultRsp

