{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}




module DRCParser
    (
      drcParser,
      main
    )
where

import Control.Applicative ((<|>), (<$>), some, many ) -- JA
import Data.Attoparsec.Text as A(Parser, Number, skipSpace, digit, char,  number  {- double, rational, decimal, signed-} , decimal, string, anyChar, takeWhile1, takeWhile, takeTill, letter, option)
import Data.Functor (($>))
-- import Data.Text (unpack)

import Lib (    PCBFeatureItem(..), DRCError(..), PCBFeature(..)  )
-- import Lib

-------


import Text.RawString.QQ
import Data.Either
import Data.Attoparsec.Text (Number(I, D), parseOnly)

import Data.Text as T -- append, concat
import Data.Text.IO as T

import Data.Char (isAlpha, isDigit, isSpace, ord)
import Data.List(intersperse)



-- fix ths, we are calling it duplicate

lexeme :: Parser a -> Parser a
lexeme p = do
    skipSpace
    p






parseNetclass :: Parser Text
parseNetclass = do
  {-
    [Net-(U102-Pad11)]
    [<no net>]
  -}
  skipSpace
  lexeme $ char '['
  netClass <- takeTill $ (==) ']'
  lexeme $ char ']'
  return netClass




-- could use Either(Left,Right) here, if wanted.
-- No. because have Via also.

isDecimal :: Char -> Bool
isDecimal c = c >= '0' && c <= '9'





drcPadParser :: Parser PCBFeature
drcPadParser = do

  -- Pad 2 [ER-DGND] of U103 on F.Cu

  skipSpace
  string "Pad"
  skipSpace
  -- padNum <- decimal   --  parser returning integer.
  padNum <- takeWhile1 isDecimal

  -- parse netclass
  netClass <- parseNetclass

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




drcTHPadParser :: Parser PCBFeature
drcTHPadParser = do

  {- Through hole pad 12 [Net-(J201-Pad12)] of J201
    Through hole pad [<no net>] of H308
    -}
  skipSpace
  string "Through hole pad"
  skipSpace

  {-
    OK. so padNum is optional here (eg. for a Mounting Hole) . how do we handle this?
    OK. this works/compiles...  to handle missing value.
    alternatively create a decimal to return a Maybe/ Just value.
    -----
    easy . treat it as a string.
  -}
  -- padNum <- option (-1) decimal

  padNum <- A.takeWhile (\c -> isAlpha c || isDigit c )  -- may be empty.


  -- parse netclass
  netClass <- parseNetclass

  skipSpace
  string "of"

  -- parse component
  skipSpace
  component <- takeWhile1 (\c -> isAlpha c || isDigit c )

  return $ PadTH_ padNum netClass component
  -- return $ Pad_ padNum netClass component ""


-- prefix with 'feature' rather than 'drc' ?




drcGeomParser :: Parser PCBFeature
drcGeomParser = do

  -- Polygon on F.Cu
  -- Arc on Edge.Cuts
  -- Line on Edge.Cuts
  -- no netclass????

  skipSpace
  geometry <- string "Polygon" <|> "Arc" <|> "Line"

  skipSpace
  string "on"

  -- parse layer
  skipSpace
  layer <- takeWhile1 ( \c -> isAlpha c || isDigit c || c == '.' )


  -- Need different structure

  return $ Geom_ geometry layer



{-

222     @(250.7617 mm, 125.9692 mm): Track [AGND] on B.Cu, length 1.6078 mm
223     @(249.1539 mm, 125.9692 mm): Track [AGND] on In2.Cu, length 2.7817 mm
224 [unconnected_items]: Missing connection between items
225     Local override; Severity: error
226     @(153.0350 mm, 63.5000 mm): Zone [AGND] on F.Cu and 2 more
-}



drcZoneParser :: Parser PCBFeature
drcZoneParser = do

  -- Zone [Net-(U102-Pad11)] on F.Cu, length 1.0827 mm

  skipSpace
  string "Zone"

  -- parse netclass
  netClass <- parseNetclass

  skipSpace
  string "on"     -- keyword 'on'. not 'of'

  skipSpace
  layer <- takeWhile1 ( \c -> c /= '\n' )

  return $ Zone_ netClass layer




drcTrackParser :: Parser PCBFeature
drcTrackParser = do

  -- Track [Net-(U102-Pad11)] on F.Cu, length 1.0827 mm

  skipSpace
  string "Track"

  -- parse netclass
  netClass <- parseNetclass

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
  netClass <- parseNetclass

  skipSpace
  string "on"     -- keyword 'on'. not 'of'

  skipSpace
  layer <- takeWhile1 ( \c -> c /= '\n' )

  return $ Via_ netClass layer




pcbFeatureParser :: Parser PCBFeatureItem
pcbFeatureParser = do

  -- feature with position. eg.
  -- @(87.9625 mm, 77.4700 mm): Pad 2 [AGND] of C238 on B.Cu

  -- parse position
  skipSpace
  lexeme $ char '@'
  lexeme $ char '('
  position <- takeWhile1 ( (/=) ')' )  -- glob rest of the line.   use predicate til and isEndOfLine
  lexeme $ char ')'
  lexeme $ char ':'

  -- parse feature
  feature <- drcPadParser <|> drcTHPadParser <|> drcTrackParser <|> drcViaParser <|> drcGeomParser <|> drcZoneParser


  -- this could probably be a tuple instead .

  return $ PCBFeatureItem {

    _position = position,
    _feature = feature
    }





commentParser :: Parser ()
commentParser = do
  skipSpace
  string "**"
  takeTill ( (==) '\n' )  -- glob rest of the line .  TODO use predicate til and isEndOfLine
  return ()





drcErrorParser :: Parser DRCError
drcErrorParser = do

  -- glob comments
  many commentParser

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

  -- takeWhile1 returns text.
  -- many, some - are parser combinators.

  items <- many pcbFeatureParser

  return $ DRCError {
    _name = name,
    _explanation = explanation,
    _features = items
    }




drcParser :: Parser [ DRCError ]
drcParser = do

  many drcErrorParser











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


    [clearance]: Clearance violation (netclass 'power' clearance 0.2000 mm; actual 0.0940 mm)
    Rule: netclass 'power'; Severity: error
    @(57.6288 mm, 51.4760 mm): Track [Net-(U102-Pad11)] on F.Cu, length 1.0827 mm
    @(91.3042 mm, 81.7965 mm): Via [AGND] on F.Cu - B.Cu


    [via_dangling]: Via is not connected or connected on only one layer
    Local override; Severity: warning
    @(57.6288 mm, 51.4760 mm): Via [Net-(U102-Pad11)] on F.Cu - B.Cu


    to parse a list - we just need to parse a comment first.
  -}

  {-
  let s = [r|

    ** Drc report for /home/me/devel/kicad6/projects/dmm05/main.kicad_pcb **
    ** Created on 2023-08-05 18:21:25 **


    [via_dangling]: Via is not connected or connected on only one layer
    Local override; Severity: warning
    @(57.6288 mm, 51.4760 mm): Via [Net-(U102-Pad11)] on F.Cu - B.Cu


    [clearance]: Clearance violation (netclass 'power' clearance 0.2000 mm; actual 0.0940 mm)
    Rule: netclass 'power'; Severity: error
    @(57.6288 mm, 51.4760 mm): Track [Net-(U102-Pad11)] on F.Cu, length 1.0827 mm
    @(91.3042 mm, 81.7965 mm): Via [AGND] on F.Cu - B.Cu

    @(89.8375 mm, 74.2950 mm): Pad 1 [CRESET] of R210 on F.Cu
    @(43.8150 mm, 85.0900 mm): Through hole pad 12 [Net-(J201-Pad12)] of J201

  |]
  -}

  s <- T.readFile "data/DRC.rpt"

  T.putStrLn s;

  let exprParseResult = parseOnly drcParser s

  if isLeft exprParseResult
    then do
      T.putStrLn $ "not a valid experssion or statemet"
    else do

      let Right expr = exprParseResult

      mapM ( Prelude.putStrLn .  show ) expr

      return ()






{-
output :: DRCExpr ->  IO ()
output expr = do

  T.putStr " "
  case expr of

    DRCError_  b  -> Prelude.putStr $ show b
-}


