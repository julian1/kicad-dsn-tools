
{-# LANGUAGE QuasiQuotes,  OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeSymboliables #-}

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

  - we have nets with brackets in them, which are not being treated correctly.
  - done - change name Symbol to Symbol
  - need a integer parser

 "Net-(C1201-Pad1)"
-}


output :: Expr ->  IO ()
output expr = do

  T.putStr " "
  case expr of

      DoubleLit x -> Prelude.putStr $ show x

      SignedLit x -> Prelude.putStr $ show x


      SingleQuote -> T.putStr "SINGLEQUOTE"

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


  -- s <- T.readFile "data/test01.sexpr"
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




{-
  -- either :: (a -> c) -> (b -> c) -> Either a b -> c
  either
    (\x ->
        T.putStrLn $ "not a valid experssion or statemet"
    )

    (\x ->
          T.putStrLn "ok"
          -- putStrLn $ show  exprParseResult

          -- // let Right expr = exprParseResult
          -- output x

          -- T.putStrLn "\n\ndone"
    )
  exprParseResult
-}



