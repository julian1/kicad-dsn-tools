
{-# LANGUAGE QuasiQuotes,  OverloadedStrings #-}

{-
  - usage
    -  parse and write output expr .dsn
    - takes single file argument of dsn file.
-}



module Main where



import Prelude as P
import Data.Attoparsec.Text(parseOnly )

import Data.Either
import Lib

-- import Text.RawString.QQ

import Data.Text as T
import Data.Text.IO as T


import System.IO(stdout)
import System.Environment (getArgs)

----

import ExprParser(exprParser)
import ExprPrint(exprPrint)







main :: IO ()
main =  do

  args <- getArgs                  -- IO [String]
  mapM P.putStrLn args

  let file = P.head args


  dsn <- T.readFile file


  {- eg. (or 1 1),    (+ 1 (+ 1 1 ))
      putStrLn "enter an expression!"
      hFlush stdout
      ls <- getLine
  -}

  let exprParseResult = parseOnly exprParser dsn

{-
  if isLeft exprParseResult
    then do
      T.putStrLn $ "not a valid experssion or statemet"
    else do
      -- T.putStrLn "ok"
      -- putStrLn $ show  exprParseResult

      let Right expr = exprParseResult


      exprPrint stdout 0 expr

      -- T.putStrLn "\n\ndone"
  -}

  either (\_ -> do
      T.putStrLn $ "not a valid expr"
    )
    ( \expr -> do

        exprPrint stdout 0 expr

        T.hPutStrLn stdout "" -- newline

    ) exprParseResult







