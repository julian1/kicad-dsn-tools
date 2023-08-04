{-# LANGUAGE OverloadedStrings #-}

-- combine the 'exprParse' functions into one module

{-
  we can probably get the position on error, by having a catch-all that slurps up the rest of the file.
  then count the characters, number of newlines.

-}

module ExprParser
    (
      exprParser
    , lexeme
    , symbolParser
    ) where

import Control.Applicative ((<|>), some, many ) -- JA
import Data.Attoparsec.Text (Parser, skipSpace, char, double, string, anyChar, takeWhile1, letter)
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
    -- lexeme $ string "cons"


    -- OK. this works, to take a list
    -- some is one or more.
    j  <- many exprParser

    lexeme $ char ')'
    -- return (Cons expr1 expr2)
    return (List j)


stringParser = do
    lexeme $ char '"'
    j <- takeWhile1 (\c -> c /= '"' && c /= ')' )
    lexeme $ char '"'
    return (StringLit  j)


{-
  handle  quotation directive.
    (string_quote ")
-}

singleQuoteParser = do
  lexeme $ char '"'
  return (StringLit "\"")



lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p

-- | parse symboliabel


symbolParser :: Parser Expr
symbolParser = do
    skipSpace
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
    -- // return (Symbol $ unpack symbol)  -- not sure why we want to unpack here.
    return (Symbol symbol )  -- not sure why we want to unpack here.

{-


symbolParser :: Parser Expr
symbolParser = do
    skipSpace
    symbol_head <- letter
    symbol <- takeWhile1 (\c ->
        c >= 'a' && c <= 'z'
        || c >= 'A' && c <= 'Z'
        || c =='_'
        || c >= '0' && c <= '9'
        || c == '.')
    return (Symbol $ symbol_head : (unpack symbol))
-}

numParser :: Parser Expr
numParser = doubleParser

doubleParser :: Parser Expr
doubleParser = do
    skipSpace
    x <- double
    return (NumLit x)



