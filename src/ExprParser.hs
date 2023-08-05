{-# LANGUAGE OverloadedStrings #-}

-- combine the 'exprParse' functions into one module

{-
  we can probably calc the error position, by having a catch-all that globs the text of remaining file.
  then count the number of newline chars til the end.
  then subtract from file size

  see, takeText :: Parser Text

  https://hackage.haskell.org/package/attoparsec-0.14.4/docs/src/Data.Attoparsec.Text.Internal.html#Parser

  https://hackage.haskell.org/package/attoparsec-0.14.4/docs/Data-Attoparsec-Text.html              <- doc

  https://hackage.haskell.org/package/attoparsec-0.14.4/docs/src/Data.Attoparsec.Text.html#decimal    <- source.

  takeWhile1 faster than many1.

  we have to parse a double first.
  and only accept if has '.'
  otherwise parse as signed decimal.

  ---

  should test on non-spectra files also.

  ---
  A numeric type that can represent integers accurately, and floating point numbers to the precision of a Double.
  Note: this type is deprecated, and will be removed in the next major release. Use the Scientific type instead.

  Constructors
  I !Integer
  D !Double

-}

module ExprParser
    (
      exprParser
    , lexeme
    , symbolParser
    ) where

import Control.Applicative ((<|>), (<$>), some, many ) -- JA
import Data.Attoparsec.Text (Parser, Number, skipSpace, digit, char,  number  {- double, rational, decimal, signed -}, string, anyChar, takeWhile1, takeWhile, letter)
import Data.Functor (($>))
-- import Data.Text (unpack)
import Lib (Expr(..))

exprParser :: Parser Expr
exprParser = ampParser <|> numParser <|>  listParser <|> stringParser <|> symbolParser  <|> singleQuoteParser

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
        || c == '.'                 -- think issue here.
        || c == '*' || c == '-'
        || c == '@' || c == ':'
        || c == '[' || c == ']'
        || c == '/'
        || c == '~'

      )
    return (Symbol symbol )


-- a integer will be picked up as a double.
-- the only way to do this is parse integer , and check the final digit is not a dot '.'.


numParser ::  Parser Expr

numParser = do
    skipSpace
    x <- number
    return (Num x)




------------

-- NO. it cannot be OR it must include the '@' to avoid being parsed as regular number
-- special digit index with ampersand in the middle
-- must be before numParser

isDecimal :: Char -> Bool
isDecimal c = c >= '0' && c <= '9'



ampParser :: Parser Expr
ampParser = do
  skipSpace
  -- this parses correctly, but doesn't give us the concatenated result
  -- j <- takeWhile1 isDecimal *> takeWhile1 (\c -> c == '@') *> takeWhile1 isDecimal

  first     <- takeWhile1 isDecimal
  -- ampersand <- take (\c -> c == '@')
  ampersand <- char '@' -- take (\c -> c == '@')
  last      <- takeWhile1 isDecimal

  -- // return (Amp (first <|> ampersand <|> last ))
  -- return (Amp (append first last ))
  return (Amp first last )



{-
numParser :: Parser Expr
-- numParser = signedParser  <|>  doubleParser
numParser = doubleParser <|> signedParser


doubleParser :: Parser Expr
doubleParser = do
    skipSpace
    -- x <- double
    x <- rational
    return (DoubleLit x)


signedParser :: Parser Expr
signedParser = do
    skipSpace
    -- x <- signed  -- has extra arg
    -- x <- decimal   -- ok.
    x <- signed decimal   -- ok.
    return (SignedLit x)

-}


