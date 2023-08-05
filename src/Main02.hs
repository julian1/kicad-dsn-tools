{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

-- combine the 'exprParse' functions into one module

{-

- ok. the main thing is the separator ':'
  actually not sure.


[copper_edge_clearance]: Board edge clearance violation (board setup constraints edge clearance 0.0500 mm; actual 0.0000 mm)
    Rule: board setup constraints edge; Severity: error
    @(151.1300 mm, 92.0750 mm): Line on Edge.Cuts
    @(151.7650 mm, 90.8050 mm): Through hole pad [<no net>] of H311



** Found 52 unconnected pads **
[unconnected_items]: Missing connection between items
    Local override; Severity: error
    @(87.9625 mm, 77.4700 mm): Pad 2 [AGND] of C238 on B.Cu
    @(87.7625 mm, 77.4700 mm): Pad 2 [AGND] of U214 on F.Cu


  white  [   a-z/ident  ] :  text newline.
    @ newline .
    @ newline


-}

module Main
    (
      exprParser
    --, lexeme
    --, symbolParser

    ,main
    ) where


import Control.Applicative ((<|>), (<$>), some, many ) -- JA
import Data.Attoparsec.Text (Parser, Number, skipSpace, digit, char,  number  {- double, rational, decimal, signed-} , decimal, string, anyChar, takeWhile1, takeWhile, takeTill, letter)
import Data.Functor (($>))
-- import Data.Text (unpack)
import Lib (Expr(..), Foo(..) )

-------


import Text.RawString.QQ
import Data.Text.IO as T
import Data.Either
import Data.Attoparsec.Text (Number(I, D), parseOnly)

import Data.Text as T -- append, concat

import Data.Char (isAlpha, isDigit, isSpace, ord)
import Data.List(intersperse)


exprParser :: Parser Expr
-- exprParser = ampParser <|> numParser <|>  listParser <|> stringParser <|> symbolParser  <|> singleQuoteParser
exprParser = unconnectedItemParser


-- change name unconnectedItemParser
unconnectedItemParser :: Parser Expr
unconnectedItemParser = do

  skipSpace
  lexeme $ char '['
  string "unconnected_items"                  -- NICE.
  -- symbol <- takeWhile1 (\c -> c /= ']' && c /= '\n' )   -- why not match against the thing we are interested in.
  lexeme $ char ']'
  skipSpace
  lexeme $ char ':'

  explanation <- takeWhile1  ((/=) '\n' )  -- glob rest of the line, which is explanation
  -- to return both the symbol and text - we will need a slightly different structure.
  -- perhaps split them.

  -- or just stuff all the element s
  -- we may want to parse specifically 'unconnected' , to make it easier to pick out the pad stuff.

  skipSpace
  secondLine <- takeWhile1 ( (/=) '\n' )  -- glob rest of the line

 
  item1 <- itemParser
  item2 <- itemParser

  -- 

  -- so we need to return a structure here?
  -- OR. having named fields is definitely easier.

  return . Symbol . T.concat . (Data.List.intersperse ", ") $ [  explanation, secondLine ] 



itemParser :: Parser Expr
itemParser = do
  ---      @(87.9625 mm, 77.4700 mm): Pad 2 [AGND] of C238 on B.Cu
  skipSpace
  lexeme $ char '@'
  lexeme $ char '('
  position <- takeWhile1 ( (/=) ')' )  -- glob rest of the line
  lexeme $ char ')'
  lexeme $ char ':'


  skipSpace
  string "Pad"                  -- NICE.

  skipSpace
  padNum <- decimal

  skipSpace
  lexeme $ char '['
  -- netClass <- takeWhile1 ( (/=) ']' )
  netClass <- takeTill (  (==) ']' )
  lexeme $ char ']'

  skipSpace
  string "of"

  skipSpace
  component <- takeWhile1 ( \c -> isAlpha c || isDigit c )

  skipSpace
  string "on"

  skipSpace
  layer <- takeWhile1 ( \c -> isAlpha c || isDigit c || c == '.' )
 

  return $ Foo_ $ Foo {  _position = position, _padNum = padNum, _netClass = netClass, _component = component, _layer = layer  } 

  -- return . Symbol . T.concat . (Data.List.intersperse ", ") $ [  netClass, component, layer ] 
  -- so we want a structure... tuple.    with where, pad, netclass  etc.



------------

lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p





output :: Expr ->  IO ()
output expr = do

  T.putStr " "
  case expr of

      -- we could parse numbers and leave as text, without conversion.
      -- https://hackage.haskell.org/package/attoparsec-0.14.4/docs/Data-Attoparsec-Number.html
      Num (I x) -> do
        Prelude.putStr $ show x

      Num (D x) -> do
        Prelude.putStr $ show x


      Amp s1 s2 -> do
        -- T.putStr "AMP"
        T.putStr s1
        T.putStr "@"
        T.putStr s2


      -- SingleQuote -> T.putStr "SINGLEQUOTE"
      SingleQuote -> T.putStr "\""

      StringLit s -> do
          T.putStr "\""
          T.putStr s
          T.putStr "\""

      Symbol s -> do
        T.putStr s

      List xs -> do
        T.putStrLn ""
        T.putStr "("
        mapM output xs    -- ignore return value
        -- return ()
        T.putStr ")"




main :: IO ()
main =  do

  let s = [r|

      [unconnected_items]: Missing connection between items
      Local override; Severity: error
      @(87.9625 mm, 77.4700 mm): Pad 2 [AGND] of C238 on B.Cu
      @(87.7625 mm, 77.4700 mm): Pad 2 [AGND] of U214 on F.Cu
    |]


  T.putStrLn s;

  let exprParseResult = parseOnly exprParser (s)


  if isLeft exprParseResult
    then do
      T.putStrLn $ "not a valid experssion or statemet"
    else do
      -- T.putStrLn "ok"
      -- putStrLn $ show  exprParseResult

      let Right expr = exprParseResult
      output expr



{-

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



-}
