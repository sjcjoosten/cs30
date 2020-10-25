{-# LANGUAGE TemplateHaskell #-}
module CS30.Exercises.Cardinality (cardEx) where
import           CS30.Data
import           CS30.Exercises.Data
import           CS30.Exercises.Util
import           Data.List.Extra (intercalate)
import qualified Data.Map as Map
import qualified Data.Set as Set
import           Data.Functor.Identity
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char -- readFile
import qualified Text.Megaparsec.Char.Lexer as L
import           Control.Monad.Combinators.Expr

data CardExp = CardExp deriving Show
-- type CardExp a = ([Field],a)
-- $(deriveJSON defaultOptions ''CardExp)

data MathExpr = Const Integer 
              | Mult  MathExpr MathExpr  -- multiply two MathExpr's 
              | Expon MathExpr MathExpr  -- set first MathExpr to the second MathExpr power
              | Choose MathExpr MathExpr -- first MathExpr `choose` second MathExpr
              deriving Show

choose :: Integer -> Integer -> Integer
choose n r | n >  r = ((choose (n-1) r) * n) `div` (n-r)
           | n == r = 1 
           | n <  r = 0

evalExpr :: MathExpr -> Integer
evalExpr (Const x)    = x
evalExpr (Mult e1 e2) = (evalExpr e1) * (evalExpr e2)
evalExpr (Expon e1 e2) = (evalExpr e1) ^ (evalExpr e2)
evalExpr (Choose e1 e2) = (evalExpr e1) `choose` (evalExpr e2)

getNums :: MathExpr -> [Integer]
getNums (Const x)    = [x]
getNums (Mult e1 e2) = getNums e1++getNums e2
getNums (Expon e1 e2) = getNums e1++getNums e2
getNums (Choose e1 e2) = getNums e1++getNums e2


cardEx :: ExerciseType
cardEx = exerciseType "Cardinality" "L??" "Cardinality of Expression" 
            cardinality
            cardQuer 
            cardFeedback

allCards :: [Integer]
allCards = [1..99] 

cardinality :: [ChoiceTree ([[Field]], [Integer])]
cardinality = [ 
            -- cardinality of the cartesian product of two sets
           nodes [ ( [[FMath$ "|A| = "++(show n1), FText" and ", FMath$"|B| = "++(show n2)], 
                      [FMath$ "|A \\times B|"], 
                      [FMath$ (show n1), FMath$"\\cdot",  FMath$ (show n2)]
                     ]
                     , [n1 * n2, n1, n2]
                     )
                   | n1 <- allCards, n2 <- allCards]
            -- cardinality of a power set
            , nodes [ ( [[FMath"|A| ", FMath$ "= "++(show n1)], 
                        [FText "|𝒫",FMath$ "(A)", FText"|"],
                        [FMath$ "2^{"++(show n1)++"}" ]
                        ]
                     , [2^n1, 2, n1]
                     )
                   | n1 <- allCards]
            -- cardinality of set x its powerset
           , Branch [ nodes [ ( [[FMath"|A| ", FMath$"= "++(show n1)],
                                [FMath$"|A \\times ", FText"𝒫", FMath"(A)|"],
                                [FMath$ (show n1)++"\\cdot 2^{"++(show n1)++"}"]
                                ]
                              ,[n1*(2^n1), 2, n1]
                              )
                            | n1 <- allCards]
                     ] 
            -- cardinality with set builder notatino (like ex from the assignment sheet)
           , Branch [ nodes [ ( [[FMath"|B| ", FMath$ "= "++(show n1)],
                                 [FMath$"\\left|\\left\\{A\\ \\mid\\ A \\subseteq B, \\ |A| ="++(show n2)++"\\right\\}\\right|"], 
                                 [FMath$"\\binom{"++(show n1)++"}{"++(show n2)++"}"] -- e.g. \binom{10}{6}
                                ]
                              , [n1 `choose` n2, n1, n2]
                              )
                            | n1 <- allCards, n2 <- [1..n1-1] ]
                    ]
           ]

cardQuer :: ([[Field]],[Integer]) -> Exercise -> Exercise
cardQuer (quer, _solution) exer 
  = exer { eQuestion=[FText "Given "] ++ rule ++ 
                     [FText ", compute "] ++ question ++ 
                     [FFieldMath "answer"]}
                        where rule = head quer
                              question = quer!!1

list_to_string :: [Integer] -> String
list_to_string = intercalate ", " . map show

cardFeedback :: ([[Field]],[Integer]) -> Map.Map String String -> ProblemResponse -> ProblemResponse
cardFeedback (quer, sol) mStrs defaultRsp 
  = reTime $ case pr of 
      Just v -> if Set.fromList numInAns `Set.isSubsetOf` (Set.fromList allowedNums) then 
                  if (evalExpr v) ==  (head sol) then 
                    markCorrect $ defaultRsp{prFeedback = [FText"Correct! "] ++ question ++ [FMath " = "] ++ step ++ [FMath " = "] ++ [FMath (show $ head sol)]} -- TODO: show intermediate steps 
                  else
                    markWrong $ defaultRsp{prFeedback = [FText("The correct answer is ")] ++ question ++ [FMath " = "] ++ step ++ [FMath " = "] ++ [FMath(show $ head sol)] ++ 
                                                        [FText(". You wrote ")] ++ [FMath$ (mStrs Map.! "answer") ]}
                else 
                  markWrong $ defaultRsp{prFeedback = [FText("Please explain your answer better. Where did you get " ++ list_to_string notInSol  ++" from?")]} 
                where numInAns    = getNums v 
                      allowedNums = tail sol
                      notInSol    = filter (\x -> notElem x allowedNums) numInAns
                     
      Nothing -> markWrong $ defaultRsp{prFeedback = [FText("The correct answer is ")] ++ question ++ [FMath " = "] ++ step ++ [FMath " = "] ++ [FMath (show $ head sol)] ++ [FText". You didn't write anything."]}
    where usr = Map.lookup "answer" mStrs
          question = quer!!1
          step = quer!!2
          pr :: Maybe MathExpr
          pr = case usr of
                 Nothing -> Nothing
                 Just v -> case parse parseExpr "" v of
                             Left _ -> Nothing -- error (errorBundlePretty str)
                             Right st -> Just st 
    
type Parser = ParsecT Void String Identity

spaceConsumer :: Parser ()
spaceConsumer = L.space spaces empty empty 

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

lexeme :: Parser a -> Parser a
lexeme   = L.lexeme spaceConsumer

-- based on Drill 6.2 scaffold
spaces :: Parser ()
spaces = some spc >> return ()
 where spc = string " " <|> string "\t" <|> string "\n" <|> string "\r"
             <|> string "\\ " <|> string "~"

brackets :: Parser a -> Parser a
brackets = between (symbol "{") (symbol "}")

parens :: Parser a -> Parser a          
parens = between (symbol "\\left(") (symbol "\\right)")

parseConstant :: Parser MathExpr
parseConstant = do
  n <- lexeme L.decimal
  return (Const n)

operatorTable :: [[Operator Parser MathExpr]]
operatorTable =
  [ [binary "^" Expon] ,
    [binary "\\cdot" Mult] ,
    [binary "choose" Choose]
  ]

binary :: String -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ symbol name)

parseBinom :: Parser MathExpr
parseBinom = do
  _  <- symbol "\\binom" 
  e1 <- brackets parseExpr 
  e2 <- brackets parseExpr
  return (Choose e1 e2)

parseExpr :: Parser MathExpr
parseExpr =  makeExprParser parseTerm operatorTable 

parseTerm :: Parser MathExpr
parseTerm = parens parseExpr <|> brackets parseExpr <|> parseConstant <|> parseBinom

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

