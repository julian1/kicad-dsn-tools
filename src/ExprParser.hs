
{-# lANGUAGE OverloadedStrings, BangPatterns #-}


-- change name SpecctraParser

-- combine the 'exprParse' functions into one module



{-
  format,
  https://cdn.hackaday.io/files/1666717130852064/specctra.pdf

  https://docs.kicad.org/doxygen/classDSN_1_1PCB.html

  freerouting,
    designformats/specctra/SpecctraFileDescription.flex


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

  ---


  Parser is a monad, so you can just inspect the return value and fail if the length's not right:

  takeWhileLo :: (Char -> Bool) -> Int -> Parser Text
  takeWhileLo f lo = do
    text <- takeWhile f
    case T.compareLength text lo of
      LT -> empty
      _  -> return text

  compareLength is from the text package. It's more efficient than comparing text's length, because compareLength may short-circuit.

    https://stackoverflow.com/questions/31146547/how-can-i-write-a-more-general-but-efficient-version-of-attoparsecs-takewhile




    vowel = inClass "aeiou"
    Range notation is supported.
    halfAlphabet = inClass "a-nA-N"
      should use inclass.  as faster


-}




module ExprParser
    (
      exprParser
    -- , lexeme
    , symbolParser
    ) where

import Control.Applicative ((<|>), (<$>), some, many ) -- JA
import Data.Attoparsec.Text as AT (Parser, Number, skipSpace, digit, char,  number  {- double, rational, decimal, signed -}, string, anyChar, takeWhile1, takeWhile, letter, satisfy, option, choice, asciiCI, many1, takeText )
import Data.Functor (($>))
-- import Data.Text (unpack)
import Lib (Expr(..))

import Data.Text as T





exprParser :: Parser Expr
exprParser = specialIndexParser <|> uuidParser <|> numParser <|>  listParser <|> stringLiteralParser <|> symbolParser  <|> singleQuoteParser




-- doesn't seem to work.
-- if add it at the end.
remainParser :: Parser Expr
remainParser = do

    j <- AT.takeText
    return (Rest j)





listParser :: Parser Expr
listParser = do

    AT.skipSpace
    char '('

    j  <- many exprParser

    skipSpace
    char ')'

    return (List j)





stringLiteralParser :: Parser Expr
stringLiteralParser = do

    skipSpace

    -- must use takeWhile1, to return false on lhs, to force the second part expression to evaluate as alternative
    -- must use 'many' (zero or more). rather than 'many1'. to pick up an empty string. ie.

    char '"'
    j <- mconcat <$> many
      (   AT.takeWhile1 (\c -> c /= '"' && c /= '\\' )
        <|>  string "\\\""
        <|>  string "\\n"
      )
    char '"'
    return (StringLit j)





{-
  to handle quotation directive.
    (string_quote ")
-}

singleQuoteParser :: Parser Expr
singleQuoteParser = do

  skipSpace
  char '"'
  return SingleQuote




uuidParser :: Parser Expr
uuidParser = do

  -- simpler to parse this explicitly. using the keyword
  -- instead of trying to match the form 6c - 4c - 4c -4c -  12c
  skipSpace
  prefix <- string "uuid" <|> string "tstamp"

  skipSpace
  val <- AT.takeWhile (\c ->
      c >= 'a' && c <= 'z'
      || c >= 'A' && c <= 'Z'
      || c >= '0' && c <= '9'
      || c == '-'
    )

  return (Uuid prefix val )





{-
  perhaps should return a string, and let the caller add the data constructor
  since this is the lowest priority construction  - we could just glob anything char here - except for a single quote char.
-}
symbolParser :: Parser Expr
symbolParser = do
    skipSpace
    -- we should probably just take anything here whitespace


    -- use 'satisfy' for first character prefix
    -- rename first/rest

    -- (layers *.Cu *.Mask)

    prefix <- satisfy (\c ->
        c >= 'a' && c <= 'z'
        || c >= 'A' && c <= 'Z'
        || c =='*'
        -- || c =='_'
      )

    suffix <- AT.takeWhile (\c ->
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
        || c == '&'     --   (layers F&B.Cu)
        -- || c == '{' || c == '}'

      )
    return (Symbol (prefix `T.cons` suffix ) )




numParser ::  Parser Expr
numParser = do
    skipSpace

    -- cannot find a 'zero or one' parsing action.
    -- use choice [  ] with a true. pure True.

    -- using choice, and a guaranteed value - forces us to handle/test the return value
    -- use mconcat
    -- j <- choice [ char '+', char '-', pure ' ' ]

    -- this doesn't work. not sure why.
    -- x <- AT.takeWhile ( (==) '+') <|>  AT.takeWhile ( (==) '-' )


    x <- AT.takeWhile (\c -> c == '+' || c == '-' )   -- takeWhile is 'zero or more' while we want 'zero or one'.  could add predicate that one char was taken only.

    -- Must have at least one digit.
    y <- takeWhile1 isDecimal
    -- optional decimal point
    z <- AT.takeWhile (\c -> c == '.' )
    w <- AT.takeWhile isDecimal

    return (Num ( T.concat [ x, y,z,w ] ) )



    -- return (Num ( T.concat $ mconcat [ [ x], y,z,w ] ) )
    -- *> drops the value? weird.
    -- <$> is for composing functions.
   {-
    x <- AT.takeWhile (\c -> c == '+' || c == '-' )   -- takeWhile is 'zero or more' while we want 'zero or one'.  could add predicate that one char was taken only.
      (.*>)
      takeWhile1 isDecimal
      (.*>)
      AT.takeWhile (\c -> c == '.' )
      (.*>)
      AT.takeWhile isDecimal

      return $ Num x
    -}




isDecimal :: Char -> Bool
isDecimal c = c >= '0' && c <= '9'



specialIndexParser :: Parser Expr
specialIndexParser = do
  skipSpace
  -- must include the '@' char, to avoid being parsed as regular number
  -- this parses correctly, but doesn't give us the concatenated result

  first     <- takeWhile1 isDecimal
  ampersand <- char '@'
  last      <- takeWhile1 isDecimal
  -- next char expected to be whitespace.

  return (SpecialIndex $ T.concat [ first, "@", last ]  )








{-
lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p
-}




{-
  it doesn't like the comma.
  probably something previous - is getting it screwed u



  (property "Datasheet" "" (id 3) (at 0 0 0)          <- it is the empty string. that is the problem
    (effects (font (size 1.27 1.27)) hide)
  )



  (property "ki_description"   "bbb,aaa"  (id 5) (at 0 0 0)
    (effects (font (size 1.27 1.27)) hide)
  )

-}

-- need many.  and T.concat.




-- https://stackoverflow.com/questions/35300812/fast-parsing-of-string-that-allows-escaped-characters
-- https://stackoverflow.com/questions/75403532/efficient-attoparsec-parser-combinating-general-parsers-and-anychar




{-
    -- this works when when input has a single escaped quote
    char '"'         -- start literal
    A.takeWhile (\c -> c /= '"' && c /= '\\')       -- consume any char except end literal, or start of escape '\'
    string "\\\""                                    -- eat escaped quote
    A.takeWhile (\c -> c /= '"')                    -- take the rest
    char '"'         -- end literal
-}
    -- attempt to generalize to handle string literal with multiple escaped chars. does not work.
    -- any use of 'many'  just hangs with 100% cpu.




{-


{-
    j <- many1 $
          A.takeWhile (\c -> c /= '"' && c /= '\\')       -- consume any char except end literal, or start of escape '\'
          <|> string "\\\""                                    -- eat escaped quote
-}




    -- OK. many just ends in an infinite loop.

    j <- many1 $ (A.takeWhile (\c -> c /= '"' && c /= '\\'   )     -- while not end quote or \
          <|>
          do
            lexeme $ char '\\'
            lexeme $ char '"'
            A.takeWhile (\c -> c /= '"'   )
          )




whoot :: Parser Expr
whoot = do
    -- string "\\\"" <|>  A.takeWhile (\c -> c /= '"' {- && c /= '\n' -} )
  A.takeWhile (\c -> c /= '"' {- && c /= '\n' -} )

-}





{-
--import Data.Attoparsec.Text as AT
-- import qualified Data.Text as T
-- import Data.Text (Text)


escaped, quoted, brackted :: Parser Text
normal =  A.takeWhile (/= '\\' )
-- normal =  A.takeWhile (\c -> c /= '\\' && c /= '\n'  )
escaped = do r <- normal
             rs <- many escaped'
             return $ T.concat $ r:rs
  where escaped' = do r1 <- normal
                      r2 <- quoted <|> brackted
                      return $ r1 <> r2

quoted = do string "\\\""
            res <- normal
            string "\\\""
            return $ "\""<>res <>"\""

brackted = do string "\\["
              res <- normal
              string "\\]"
              return $ "["<>res<>"]"


-}





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


