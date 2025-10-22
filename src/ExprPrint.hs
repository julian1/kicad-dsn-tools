
{-# LANGUAGE OverloadedStrings #-}
-- {-# LANGUAGE NamedFieldPuns #-}



module ExprPrint
    (
      exprPrint
      --, main
    )
where



import System.IO(Handle())
import Data.Text as T -- justifyRight
import Data.Text.IO as T

----------

import Lib



exprPrint ::  Handle -> Int -> Expr  ->  IO ()
exprPrint h level expr = do

  -- this can be our recursive walk of the dsn function. that tests membership
  T.hPutStr h " "

  case expr of

    List xs -> do
      -- handle indentation
      T.hPutStrLn h ""   -- new line.
      let pad = T.justifyRight (level * 2 ) ' ' T.empty -- pad.
      T.hPutStr h pad

      -- recurse on child items
      T.hPutStr h "("
      mapM_ (exprPrint h (level + 1)) xs
      T.hPutStr h ")"

    Symbol s -> do
      -- T.hPutStr h "S:"
      T.hPutStr h s

    Num s -> do
      -- T.hPutStr h "N:"
      T.hPutStr h s

{-
    Uuid prefix val -> do
      -- uuid or tstamp
      -- T.hPutStr h "U:"
      T.hPutStr h prefix
      T.hPutStr h " "
      T.hPutStr h val
-}

    SpecialIndex s -> do
      T.hPutStr h s

    -- SingleQuote -> T.putStr "SINGLEQUOTE"
    SingleQuote -> do
      T.hPutStr h "\""

    StringLit s -> do
      T.hPutStr h "\""
      T.hPutStr h s
      T.hPutStr h "\""


{-

  -- we could parse numbers and leave as text, without conversion.
  -- https://hackage.haskell.org/package/attoparsec-0.14.4/docs/Data-Attoparsec-Number.html
  -- We don't have to decode these in the parser.
  Num (I x) -> do

    Prelude.putStr $ show x

  Num (D x) -> do
    Prelude.putStr $ show x

  Num s -> do
    T.putStr "{"
    T.putStr s
    T.putStr "}"

  -- Need a much better name for this.
  -- qualified index.
  -- Or use a (Integer,  Maybe Integer ).
  SpecialIndex s -> do
    -- T.putStr "AMP"
    T.putStr s

  -- SingleQuote -> T.putStr "SINGLEQUOTE"
-}
