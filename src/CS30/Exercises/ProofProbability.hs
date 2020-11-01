module CS30.Exercises.ProofProbability where
import           Text.Megaparsec
import           Text.Megaparsec.Char
import           CS30.Exercises.Probability.SolutionChecker
import Control.Monad.Combinators.Expr
import qualified Text.Megaparsec.Char.Lexer as L


data Fractional_expr = FConst Rational | Probexpr Event_exper Event_exper

data Event_exper = BaseEvent Char 

-- parser single char for basic event
event_parser :: Parser Event_exper
event_parser = do {lt_spaces; 
                    c <- satisfy isLetterforEvent;
                    _ <- lt_spaces;
                    return (BaseEvent c)}
                where isLetterforEvent v = elem v ['A'..'Z']

fractional_parser :: Parser Fractional_expr
fractional_parser = do {lt_spaces;
                    do {r <- (string "0"*> return 0) <|> (string "1"*> return 1);
                        lt_spaces;
                        return (FConst r)}
                    <|>
                    do {string "Pr[";
                        e1 <- event_parser;
                        string "|";
                        e2 <- event_parser;
                        string "]";
                        lt_spaces;
                        return (Probexpr e1 e2)
                    }
                    }

data MathExpr = Const Rational 
              | Divide MathExpr MathExpr  -- division
              | Mult  MathExpr MathExpr  -- multiply two MathExpr's 
              deriving Show

-- ^here lets us parse things like (3)(3) = 3*3
operatorTable :: [[Operator Parser MathExpr]]
operatorTable = [[binary "\\cdot" Mult, binary "" Mult]]

binary :: String -> (a -> a -> a) -> Operator Parser a
binary name f = InfixL (f <$ symbol name)

symbol :: String -> Parser String
symbol = L.symbol spaceConsumer

spaceConsumer :: Parser ()
spaceConsumer = L.space spaces empty empty 

spaces :: Parser ()
spaces = some spc >> return ()
 where spc = string " " <|> string "\t" <|> string "\n" <|> string "\r"
             <|> string "\\ " <|> string "~"
