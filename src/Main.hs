
{-# LANGUAGE QuasiQuotes,  OverloadedStrings #-}
-- {-# LANGUAGE ScopedTypeSymboliables #-}

module Main where



import Data.Attoparsec.Text


import Data.Attoparsec.Number

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
  designformats/specctra/Package.java:                    System.out.println("Package.read_pin_info: number expected");

  - ampersand is used for specifying duplicate pins. so value is an index/integer.
  - or probably needs special case handling.

  247 ( pin Round[A]Pad_2700_um 1 0 -20300)
  248 ( pin Round[A]Pad_2700_um 1@1 0 0)
  249 ( pin Round[A]Pad_2700_um 2 52500 -20300)
  250 ( pin Round[A]Pad_2700_um 2@1 52500 0)) 
-}


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


  -- s <- T.readFile "data/test01.sexpr"
  -- s <- readFile "data/test02.sexpr"
  s <- T.readFile "data/main.dsn"
  -- s <- T.readFile "data/main-simple.dsn"

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
      -- T.putStrLn "ok"
      -- putStrLn $ show  exprParseResult

      let Right expr = exprParseResult
      output expr

      -- T.putStrLn "\n\ndone"




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



