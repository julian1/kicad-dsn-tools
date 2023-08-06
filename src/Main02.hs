
{-# LANGUAGE QuasiQuotes, OverloadedStrings #-}

import Data.Either
-- import Data.Text


import Data.Text as T -- append, concat
import Data.Text.IO as T

import Data.Attoparsec.Text (Number(I, D), parseOnly)

-- import Text.RawString.QQ
---------------
import Lib
import DRCParser(drcParser )


main :: IO ()
main =  do


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




