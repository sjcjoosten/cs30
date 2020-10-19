{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall #-}

module CS30.Exercises.ParseTest where
import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators.Expr-- (makeExprParser)

{-
Reference: 
  https://hackage.haskell.org/package/parser-combinators-1.2.1/docs/Control-Monad-Combinators-Expr.html#v:makeExprParser
  https://hackage.haskell.org/package/megaparsec-9.0.0/docs/Text-Megaparsec-Char-Lexer.html
-}

expr = makeExprParser term table <?> "expression"

-- symbol could be L.symbol, not sure
parens = between (symbol "(") (symbol ")")

-- how to incorporate literals (P/Q/R/S)?
term = parens expr <|> integer <?> "term"

-- binary could either be CS30.Exercises.ParseTest.binary or L.binary, not sure
table = [ 
          [CS30.Exercises.ParseTest.prefix  "~"  (CS30.Exercises.ParseTest.not)],
          [CS30.Exercises.ParseTest.binary  "&"  (CS30.Exercises.ParseTest.and)],
          [CS30.Exercises.ParseTest.binary  "|"  (CS30.Exercises.ParseTest.or)]
        ]

binary  name f = InfixL  (f <$ char name)
prefix  name f = Prefix  (f <$ char name)

-- defined these since the reference website seemed to supply functions

not :: Bool -> Bool
not x = if x == True then False else True

and :: Bool -> Bool -> Bool
and x y = (x == True && y == True)

or :: Bool -> Bool -> Bool
or x y = CS30.Exercises.ParseTest.not (x == False && y == False)
