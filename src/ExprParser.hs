
{-# lANGUAGE OverloadedStrings, BangPatterns #-}


-- change name SpecctraParser

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
import Data.Attoparsec.Text as A (Parser, Number, skipSpace, digit, char,  number  {- double, rational, decimal, signed -}, string, anyChar, takeWhile1, takeWhile, letter, satisfy, option, choice )
import Data.Functor (($>))
-- import Data.Text (unpack)
import Lib (Expr(..))

import Data.Text as T

exprParser :: Parser Expr
exprParser = specialIndexParser <|> numParser <|>  listParser <|> stringParser <|> symbolParser  <|> singleQuoteParser

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
    j <- A.takeWhile (\c -> c /= '"' && c /= '\n'  )
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


-- this is dumb. should return a string, and let the caller do the data constructor
symbolParser :: Parser Expr
symbolParser = do
    skipSpace
    -- could probably be anything except whitespace
    symbol <- takeWhile1 (\c ->
        c >= 'a' && c <= 'z'
        || c >= 'A' && c <= 'Z'
        || c =='_'
        || c >= '0' && c <= '9'
        || c == '.'                 -- be careful can match double
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
-- numParser ::  Parser Text

numParser = do
    skipSpace

    -- cannot find a 'zero or one' parsing action.
    -- use choice [  ] with a true. pure True.

    -- using choice, and a guaranteed value - forces us to handle/test the return value
    -- use mconcat
    -- j <- choice [ char '+', char '-', pure ' ' ]

    -- this doesn't work. not sure why.
    -- x <- A.takeWhile ( (==) '+') <|>  A.takeWhile ( (==) '-' )


    x <- A.takeWhile (\c -> c == '+' || c == '-' )   -- takeWhile is 'zero or more' while we want 'zero or one'.  could add predicate that one char was taken only.

    -- Must have at least one digit.
    y <- takeWhile1 isDecimal
    -- optional decimal point
    z <- A.takeWhile (\c -> c == '.' )
    w <- A.takeWhile isDecimal

    return (Num ( T.concat [ x, y,z,w ] ) )



    -- return (Num ( T.concat $ mconcat [ [ x], y,z,w ] ) )
    -- *> drops the value? weird.
    -- <$> is for composing functions.
   {-
    x <- A.takeWhile (\c -> c == '+' || c == '-' )   -- takeWhile is 'zero or more' while we want 'zero or one'.  could add predicate that one char was taken only.
      (.*>)
      takeWhile1 isDecimal
      (.*>)
      A.takeWhile (\c -> c == '.' )
      (.*>)
      A.takeWhile isDecimal

      return $ Num x
    -}




isDecimal :: Char -> Bool
isDecimal c = c >= '0' && c <= '9'



specialIndexParser :: Parser Expr
specialIndexParser = do
  skipSpace
  -- must include the '@' char, to avoid being parsed as regular number
  -- this parses correctly, but doesn't give us the concatenated result
  -- j <- takeWhile1 isDecimal *> takeWhile1 (\c -> c == '@') *> takeWhile1 isDecimal

  first     <- takeWhile1 isDecimal
  ampersand <- char '@'
  last      <- takeWhile1 isDecimal

  -- // return (SpecialIndex (first <|> ampersand <|> last ))
  -- return (SpecialIndex (append first last ))
  return (SpecialIndex $ T.concat [ first, "@", last ]  )











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


