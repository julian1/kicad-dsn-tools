
{-# LANGUAGE QuasiQuotes,  OverloadedStrings #-}

module Main where



import Data.Attoparsec.Text
import System.IO
import Data.Either
import Data.Text
import Lib
import ExprParser

import Text.RawString.QQ


-- if all the elements are list elements then print it flat...

output :: Expr ->  IO () 
output expr = do

  putStr " "
  case expr of 

      NumLit x -> putStr $ show x

      StringLit s -> putStr $ show s

      Var s -> putStr $ show s

      List xs -> do   
        putStrLn "" -- // new line
        putStr "("
        mapM output xs 
        return ()

        putStr ")"







main :: IO ()
main =  do

{-
  let s = [r|
    (blue xxx "app" 1 (+ 1 1 ) )
   |]
-}

  -- // readFile :: FilePath -> IO Text
  -- Data.Text.IO
  -- s <- readFile "data/test02.sexpr"
  s <- readFile "data/main.dsn"

  -- putStrLn s


  -- eg. (or 1 1),    (+ 1 (+ 1 1 ))
  --putStrLn "enter an expression!"
  -- hFlush stdout
  -- ls <- getLine

  let exprParseResult = parseOnly exprParser (pack s)


  if isLeft exprParseResult
    then do
        putStrLn $ "not a valid experssion or statemet"
    else do
      putStrLn "ok"
      -- putStrLn $ show  exprParseResult

      let Right expr = exprParseResult
      output expr

      putStrLn "done"
      


