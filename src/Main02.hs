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
      -- drcParser

    main
    ) where


import Control.Applicative ((<|>), (<$>), some, many ) -- JA
import Data.Attoparsec.Text (Parser, Number, skipSpace, digit, char,  number  {- double, rational, decimal, signed-} , decimal, string, anyChar, takeWhile1, takeWhile, takeTill, letter)
import Data.Functor (($>))
-- import Data.Text (unpack)

import Lib (    PCBFeatureItem(..), DRCError(..), PCBFeature(..)  )
-- import Lib

-------


import Text.RawString.QQ
import Data.Text.IO as T
import Data.Either
import Data.Attoparsec.Text (Number(I, D), parseOnly)

import Data.Text as T -- append, concat

import Data.Char (isAlpha, isDigit, isSpace, ord)
import Data.List(intersperse)







-- could use Either(Left,Right) here, if wanted.
-- No. because have Via also.

drcPadParser :: Parser PCBFeature
drcPadParser = do

  -- Pad 2 [ER-DGND] of U103 on F.Cu

  skipSpace
  string "Pad"
  skipSpace
  padNum <- decimal   --  parser returning integer.


  -- parse netclass
  skipSpace
  lexeme $ char '['
  netClass <- takeTill $ (==) ']'
  lexeme $ char ']'

  skipSpace
  string "of"


  -- parse component
  skipSpace
  component <- takeWhile1 ( \c -> isAlpha c || isDigit c )

  skipSpace
  string "on"

  -- parse layer
  skipSpace
  layer <- takeWhile1 ( \c -> isAlpha c || isDigit c || c == '.' )


  return $ Pad_ padNum netClass component layer





drcTrackParser :: Parser PCBFeature
drcTrackParser = do

  -- Track [Net-(U102-Pad11)] on F.Cu, length 1.0827 mm

  skipSpace
  string "Track"

  -- parse netclass
  skipSpace
  lexeme $ char '['
  netClass <- takeTill $ (==) ']'
  lexeme $ char ']'

  skipSpace
  string "on"     -- keyword 'on'. not 'of'

  skipSpace
  layer <- takeWhile1 ( \c -> c /= ',' )

  -- treat lenth as string for the moment
  lexeme $ char ','
  skipSpace
  length <- takeWhile1 ( \c -> c /= '\n' )

  return $ Track_ netClass layer  length



drcViaParser :: Parser PCBFeature
drcViaParser = do

  -- Via [AGND] on F.Cu - B.Cu

  skipSpace
  string "Via"

  -- parse netclass
  skipSpace
  lexeme $ char '['
  netClass <- takeTill $ (==) ']'
  lexeme $ char ']'

  skipSpace
  string "on"     -- keyword 'on'. not 'of'


  skipSpace
  layer <- takeWhile1 ( \c -> c /= '\n' )


  -- return $ Track_ "whoot Via" "x" "y"
  return $ Via_ netClass layer




pcbFeatureParser :: Parser PCBFeatureItem
pcbFeatureParser = do

  -- @(87.9625 mm, 77.4700 mm): Pad 2 [AGND] of C238 on B.Cu

  -- parse position
  skipSpace
  lexeme $ char '@'
  lexeme $ char '('
  position <- takeWhile1 ( (/=) ')' )  -- glob rest of the line
  lexeme $ char ')'
  lexeme $ char ':'

  -- parse feature
  feature <- drcPadParser <|> drcTrackParser <|> drcViaParser


  -- this could probably be a tuple instead .

  return $ PCBFeatureItem {

    _position = position,
    _feature = feature
    }




-- change name drcErrorParser
drcErrorParser :: Parser DRCError
drcErrorParser = do

  skipSpace
  lexeme $ char '['
  -- string "unconnected_items"                  -- NICE.
  name <- takeWhile1 (\c -> c /= ']' && c /= '\n' )   -- why not match against the thing we are interested in.
  lexeme $ char ']'
  skipSpace
  lexeme $ char ':'

  explanation <- takeTill ( (==) '\n' )  -- glob rest of the line

  skipSpace
  secondLine <- takeTill ( (==) '\n' )  -- glob rest of the line

  item1 <- pcbFeatureParser
  item2 <- pcbFeatureParser


  return $ DRCError {
    _name = name,
    _explanation = explanation,
    _item1 = item1,
    _item2 = item2
    }





------------

lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p




main :: IO ()
main =  do

{-
  let s = [r|

      [unconnected_items]: Missing connection between items
      Local override; Severity: error
      @(87.9625 mm, 77.4700 mm): Pad 2 [AGND] of C238 on B.Cu
      @(87.7625 mm, 77.4700 mm): Pad 2 [AGND] of U214 on F.Cu
    |]
-}

  {-
  -- clearance error looks almost the same.  except has 'Track' instead of Pad.
  --  if we want a subparser to preserve the text. then we need a simple Text parser.
  -- and then we subsequently destructure.

  - OK. so there is also a via.

    [clearance]: Clearance violation (netclass 'power' clearance 0.2000 mm; actual 0.1183 mm)
    Rule: netclass 'power'; Severity: error
    @(89.9547 mm, 81.2800 mm): Track [LP3V3] on F.Cu, length 0.8503 mm
    @(91.3042 mm, 81.7965 mm): Via [AGND] on F.Cu - B.Cu

  -}

  let s = [r|

    [clearance]: Clearance violation (netclass 'power' clearance 0.2000 mm; actual 0.0940 mm)
    Rule: netclass 'power'; Severity: error
    @(57.6288 mm, 51.4760 mm): Track [Net-(U102-Pad11)] on F.Cu, length 1.0827 mm
    @(91.3042 mm, 81.7965 mm): Via [AGND] on F.Cu - B.Cu

  |]


  T.putStrLn s;

  let exprParseResult = parseOnly drcErrorParser s


  if isLeft exprParseResult
    then do
      T.putStrLn $ "not a valid experssion or statemet"
    else do

      let Right expr = exprParseResult

      Prelude.putStr $ show expr






{-
output :: DRCExpr ->  IO ()
output expr = do

  T.putStr " "
  case expr of

    DRCError_  b  -> Prelude.putStr $ show b
-}


