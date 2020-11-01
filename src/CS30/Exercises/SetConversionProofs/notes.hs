{-
authors: Donia Tung
COSC 69.14, 20F
Group Assignment 2
-}

-- module CS30.Exercises.SetConversionProofs.LawParser where
-- import Text.Megaparsec
-- import Text.Megaparsec.Char
-- import Data.Void
-- import           Data.Functor.Identity
-- import qualified Text.Megaparsec.Char.Lexer as L
-- import           Control.Monad.Combinators.Expr

-- type Parser = ParsecT Void String Identity

-- data GExpr opSet = Var String | Op opSet [GExpr opSet]
  

-- data Law opSet = Law String (Equation opSet)
-- -- data SetExpr = Var String 
-- --                | Cap [SetExpr]
-- --                | Cup [SetExpr]
-- --                | SetMinus [SetExpr]
-- --                | Power [SetExpr]
-- --                | Wedge [SetExpr]

-- data Operation = Cap 
--                 | Cup 
--                 | SetMinus 
--                 | Power 
--                 | Wedge 
--                 deriving (Show, Eq)


-- type Equation opSet = (GExpr opSet, GExpr opSet)
-- -- data Proof opSet = Proof(GExpr opSet)[(String, GExpr opSet)]

-- --create a data structure to store, for all, take expressions, expressions will need operators 


-- parseUntil :: Char -> Parser String
-- parseUntil c
--   = (do _ <- satisfy (== ':')
--         return []) <|> 
--     (do c <- satisfy (const True) 
--         rmd <- parseUntil c 
--         return (c:rmd) 
--         )

-- -- upto :: Char -> Parser String
-- -- upto c
-- --     = Parser (\s ->
-- --             let (xs,ys) = break (==c) s in
-- --             if null ys then []
-- --             else [(xs,tail ys)])

-- -- name: x + y = y + x
-- parseLaw :: Parser (GExpr opSet) -> Parser (GExpr opSet) -> Parser (Law opSet)
-- parseLaw parseLeft parseRight
--     = do lawName <- parseUntil ':'
--          _ <- string ":"
--          lhs <- parseLeft
--          _ <- string "="
--          rhs <- parseRight
--          return (Law lawName (lhs, rhs))

-- parseName :: Parser (GExpr opSet) 
-- parseName = Var <$> some (satisfy f)
--    where f x = elem x "A-Z" -- lookup if that's right


-- parseOperator :: Parser Operation
-- parseOperator
--   = (string "\\cup" *> return Cup)
--     <|> (string "\\cap" *> return Cap)
--     <|> (string "\\setminus" *> return SetMinus)
--     <|> (string "\\P" *> return Power)
--     <|> (string "\\wedge" *> return Wedge)

-- binary :: String -> (a -> a -> a) -> Operator Parser a
-- binary name f = InfixL (f <$ symbol name)

-- operatorTable :: [[Operator Parser Operation]]
-- operatorTable =
--   [[binary "\\cap" Cup] ,
--    [binary "\\cup" Cup] , 
--    [binary "\\setminus" SetMinus] , 
--    [binary "\\P" Power] 
--   ]     
-- parseTerm :: Parser (GExpr opSet)
-- parseTerm = parens parseOperator <|> brackets parseExpr <|> parseConstant <|> parseBinom <|> parseFrac

-- -- parse a full expression (using makeExprParser)
-- parseExpr :: Parser (GExpr opSet)
-- parseExpr =  makeExprParser parseTerm operatorTable



-- -- parseExpr :: Parser (GExpr opSet)
-- -- parseExpr 
-- --     = do v1 <- parseName 
-- --          op <- parseOperator



-- -- Op \cap [Var A, Var B]
-- parseLeft :: Parser (GExpr opSet) 
-- parseLeft 
--     = do _ <- string "(for all"
--          v <- parseVars
--          _ <- string ")"
--          b <- parseExpr 
--          return (Op b v)

-- comma :: Parser String
-- comma = string "," <* space

-- parseVars :: Parser [GExpr opSet]
-- parseVars 
--   = parseName `sepBy` comma

-- parseRight :: Parser (GExpr opSet) 
-- parseRight = undefined
-- -- parseExpr =  makeExprParser parseTerm operatorTable

-- -- parse spaces (used w/ symbol and lexeme)
-- spaceConsumer :: Parser ()
-- spaceConsumer = L.space spaces empty empty 

-- -- based on Drill 6.2 scaffold
-- spaces :: Parser ()
-- spaces = some spc >> return ()
--  where spc = string " " <|> string "\t" <|> string "\n" <|> string "\r"
--              <|> string "\\ " <|> string "~"

-- -- parse a given specific string, accounting for spaces 
-- symbol :: String -> Parser String
-- symbol = L.symbol spaceConsumer

-- -- parse some lexeme, accounting for spaces
-- lexeme :: Parser a -> Parser a
-- lexeme   = L.lexeme spaceConsumer

-- -- parse something between {...}
-- brackets :: Parser a -> Parser a
-- brackets = between (symbol "{") (symbol "}")

-- -- parse something between (...) 
-- parens :: Parser a -> Parser a          
-- parens = between (symbol "(") (symbol ")")

-- -- parses some integer number alone into GExpr 
-- -- parseConstant :: Parser (GExpr opSet)
-- -- parseConstant = do
-- --   n <- lexeme L.decimal
-- --   return (Var n)

