
{-# LANGUAGE OverloadedStrings #-}

module Main where


import Data.Attoparsec.Text
import System.IO
import Data.Either
import Data.Text
import Lib
import ExprParser



main :: IO ()
main =  do

  putStrLn "Hello, Haskell!"

  hFlush stdout
  ls <- getLine

  let stmtParseResult = parseOnly exprParser (pack ls)

  putStrLn "whoot"
