
{-# LANGUAGE QuasiQuotes,  OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeVariables #-}

module Main where



import Data.Attoparsec.Text
-- import System.IO
import Data.Either
import Data.Text
import Lib
import ExprParser

import Text.RawString.QQ

--------
-- putStr 
import Data.Text.IO as T

-- if all the elements are list elements then print it flat...

{-

  we have nets with brackets in them, which are not being treated correctly.

 "Net-(C1201-Pad1)"
-}


output :: Expr ->  IO () 
output expr = do

  T.putStr " "
  case expr of 

      NumLit x -> Prelude.putStr $ show x

      StringLit s -> T.putStr s

      -- Var s -> putStr $ show s
      Var s -> T.putStr s

      List xs -> do   
        T.putStrLn "" -- // new line
        T.putStr "("
        mapM output xs 
        return ()

        T.putStr ")"







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
  s <- T.readFile "data/main.dsn"

  -- putStrLn s


  -- eg. (or 1 1),    (+ 1 (+ 1 1 ))
  --putStrLn "enter an expression!"
  -- hFlush stdout
  -- ls <- getLine

  let exprParseResult = parseOnly exprParser (s)


  if isLeft exprParseResult
    then do
        T.putStrLn $ "not a valid experssion or statemet"
    else do
      T.putStrLn "ok"
      -- putStrLn $ show  exprParseResult

      let Right expr = exprParseResult
      output expr

      T.putStrLn "\n\ndone"
      


