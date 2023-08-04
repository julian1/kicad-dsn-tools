{-# LANGUAGE OverloadedStrings #-}

-- combine the 'exprParse' functions into one module

{-
  we can probably calc the error position, by having a catch-all that globs the text of remaining file.
  then count the number of newline chars til the end.
  then subtract from file size

  https://hackage.haskell.org/package/attoparsec-0.14.4/docs/src/Data.Attoparsec.Text.Internal.html#Parser

  takeWhile1 faster than many1.
-}

module ExprParser
    (
      exprParser
    , lexeme
    , symbolParser
    ) where

import Control.Applicative ((<|>), some, many ) -- JA
import Data.Attoparsec.Text (Parser, skipSpace, char, double, decimal , signed, string, anyChar, takeWhile1, takeWhile, letter)
import Data.Functor (($>))
-- import Data.Text (unpack)
import Lib (Expr(..))

exprParser :: Parser Expr
exprParser = numParser <|>  listParser <|> stringParser <|> symbolParser  <|> singleQuoteParser

-- | parse bool expression

-- | parse floating-point expression


listParser :: Parser Expr
listParser = do
    lexeme $ char '('

    j  <- many exprParser

    lexeme $ char ')'
    return (List j)


stringParser = do
    lexeme $ char '"'

    -- has to handle brackets ()
    j <- Data.Attoparsec.Text.takeWhile (\c -> c /= '"' && c /= '\n'  )
    lexeme $ char '"'
    return (StringLit  j)


{-
  to handle quotation directive.
    (string_quote ")
-}
singleQuoteParser = do
  lexeme $ char '"'
  return SingleQuote



lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p



symbolParser :: Parser Expr
symbolParser = do
    skipSpace
    -- could probably be anything except whitespace
    symbol <- takeWhile1 (\c ->
        c >= 'a' && c <= 'z'
        || c >= 'A' && c <= 'Z'
        || c =='_'
        || c >= '0' && c <= '9'
        || c == '.'
        || c == '*' || c == '-'
        || c == '@' || c == ':'
        || c == '[' || c == ']'
        || c == '/'
        || c == '~'

      )
    return (Symbol symbol )



numParser :: Parser Expr
numParser = signedParser  <|>  doubleParser


doubleParser :: Parser Expr
doubleParser = do
    skipSpace
    x <- double
    return (DoubleLit x)


signedParser :: Parser Expr
signedParser = do
    skipSpace
    -- x <- signed  -- has extra arg
    -- x <- decimal   -- ok.
    x <- signed decimal   -- ok.
    return (SignedLit x)




